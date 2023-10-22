(* http://dev.stephendiehl.com/fun/006_hindley_milner.html *)

open Base
open Ast
open Ty
module Format = Stdlib.Format (* silencing a warning *)

let use_logging = false

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:('b -> int * 'a -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f acc (key, data))
    ;;
  end
  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  (* проверяет встерчается ли искомый TVar в выражении *)
  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TInt | TBool | TUnit -> false
  ;;

  (* Возвращает сет всех типовых переменых, которые встречаются в выражении*)
  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TInt | TBool | TUnit -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val pp : Stdlib.Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> ty

  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let pp ppf subst =
    let list = Map.to_alist subst in
    let open Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k pp_typ v))
      list
  ;;

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find_exn k xs = Base.Map.find_exn xs k
  let find k xs = Base.Map.find xs k
  let remove xs k = Base.Map.remove xs k

  (* Подставляет типы в выражение согласно контексту из списка и  возвращает результат замены*)
  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find b s with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | other -> other
    in
    helper
  ;;

  (* Пытается унифицировать(Также проверить совместимость двх выражений по типам) типы двух выражений и возвращает либо ошибку,
     либо результат(ассоциативный список с "номером" типа и типом) либо ошибку *)
  let rec unify l r =
    match l, r with
    | TInt, TInt | TBool, TBool -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | _ -> fail (`Unification_failed (l, r))

  (* Следующие функции помогают объеденить два ассоцитавных списка*)

  (* расширяет контекст новой переменной типа, либо добавляя ее в список и применяя к ней все из контекста, либо унифицируя ее тип*)
  and extend s (k, v) =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.add_exn acc ~key:k ~data:v))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc ss ->
      let* acc = acc in
      compose acc ss)
  ;;
end

module VarSet = struct
  include VarSet

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Scheme = struct
  type t = scheme

  (* Проверяет встречается ли в схеме (варсете и типовом выражении) данная типовая переменная *)
  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  (* Возвращает все типовые переменные, которые есть в выражении, но нет в сете*)
  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  (* Возвращает новую схему, удаляя из ассоциативного списка все что есть в varset и применяя к типовому выражению полученный список
     (насколько я понимаю это сделано для того, чтобы задавать локальный контекст)*)
  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  let pp = pp_scheme
end

module TypeEnv = struct
  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  (* Просто возвращает все переменные, которые есть во всех выражениях, но нет в сетах*)
  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) -> Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

(* Создает новые fresh_var и заменяет ими старые типовые переменные в выражении
   (Насколько я понял это нужно чтобы уточнять тип функции в каждом конкретном случае)*)
let instantiate : scheme -> ty R.t =
  fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

(* достает из окружения схему функции *)
let lookup_env e xs =
  match List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Stdlib.Not_found) | (exception Not_found_s _) -> fail (`No_variable e)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let infer =
  let rec (helper : TypeEnv.t -> Ast.expr -> (Subst.t * ty) R.t) =
    fun env -> function
    | EBinOp (bin_op, l, r) ->
      let* sl, tl = helper env l in
      let* sr, tr = helper env r in
      (match bin_op with
       | Add | Sub | Mul | Div | Mod ->
         let* s1 = unify tl int_typ in
         let* s2 = unify tr int_typ in
         let* sres = Subst.compose_all [ s1; s2; sl; sr ] in
         return (sres, int_typ)
       | Less | Leq | Gre | Geq | Eq | Neq ->
         let* s1 = unify tl tr in
         let* sres = Subst.compose_all [ s1; sl; sr ] in
         return (sres, bool_typ)
       | And | Or ->
         let* s1 = unify tl bool_typ in
         let* s2 = unify tr bool_typ in
         let* sres = Subst.compose_all [ s1; s2; sl; sr ] in
         return (sres, bool_typ))
    | EVar x -> lookup_env x env
    | EFun (p, e1) ->
      let* tv = fresh_var in
      let* env2 =
        match p with
        | PVar x -> return (TypeEnv.extend env (x, S (VarSet.empty, tv)))
        | _ -> return env
      in
      let* s, ty = helper env2 e1 in
      let trez = TArrow (Subst.apply s tv, ty) in
      return (s, trez)
    | EApp (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (TArrow (t2, tv)) in
      let trez = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, trez)
    | EConst n ->
      (match n with
       | CInt _ -> return (Subst.empty, int_typ)
       | CBool _ -> return (Subst.empty, bool_typ)
       | CUnit -> return (Subst.empty, unit_typ))
    | EIf (c, th, el) ->
      let* s1, t1 = helper env c in
      let* s2, t2 = helper env th in
      let* s3, t3 = helper env el in
      let* s4 = unify t1 bool_typ in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      R.return (final_subst, Subst.apply s5 t2)
    | ELetIn (false, id, e1, e2) ->
      let* s1, t1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t2 = generalize env2 t1 in
      let* s2, t3 = helper (TypeEnv.extend env2 (id, t2)) e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t3)
    | ELetIn (true, id, e1, e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (id, S (VarSet.empty, tv)) in
      let* s1, t1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s = Subst.compose s2 s1 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2 = helper TypeEnv.(extend (apply s env) (id, t2)) e2 in
      let* final_subst = Subst.compose s s2 in
      return (final_subst, t2)
  in
  helper
;;

let infer_prog prog = 
  let format_env env =
    let* env = env in
   return @@ List.rev_map env ~f:(fun (id, s) ->
    match s with
    | S (_, t) -> (id, t)
    )
  in
  let rec helper env prog = 
    (match prog with
    | h :: tl ->
      (match h with
      | ELet (false, id, expr) ->
        let* s, t = infer env expr in
        let env = TypeEnv.apply s env in
        let t = generalize env t in
        let env = TypeEnv.extend env (id, t) in
        Stdlib.Format.printf "%s -> %a\n" id pp_scheme t;
        helper env tl
      | ELet (true, id, expr) ->
        let* tv = fresh_var in
        let env = TypeEnv.extend env (id, S (VarSet.empty, tv)) in
        let* s, t = infer env expr in
        let* s2 = unify (Subst.apply s tv) t in
        let* s = Subst.compose s2 s in
        let env = TypeEnv.apply s env in
        helper env tl
      )
    | [] -> return env)

    in format_env (helper TypeEnv.empty prog)


let w e = Result.map (run (infer TypeEnv.empty e)) ~f:snd

let run_prog_inference prog = run(infer_prog prog) 

let print_prog_result prog = 
  match run_prog_inference prog with
  | Ok l -> List.iter l ~f:(fun (id, t) -> Stdlib.Format.printf "%s -> %a\n" id pp_typ t);
  | Error e -> print_typ_err e

let print_result expression =
  match w expression with
  | Ok typ -> print_typ typ
  | Error e -> print_typ_err e
;;

let%expect_test _ =
  print_result
    (ELetIn
       ( true
       , "fac"
       , EFun
           ( PVar "n"
           , EIf
               ( EBinOp (Leq, EVar "n", EConst (CInt 1))
               , EConst (CInt 1)
               , EBinOp
                   ( Mul
                   , EBinOp (Sub, EVar "n", EConst (CInt 1))
                   , EApp (EVar "fac", EBinOp (Sub, EVar "n", EConst (CInt 1))) ) ) )
       , EApp (EVar "fac", EConst (CInt 5)) ));
  [%expect {| int |}]
;;

let%expect_test _ =
  print_result (EFun (PVar "x", EFun (PVar "y", EFun (PVar "z", EConst (CInt 5)))));
  [%expect {| 'a -> 'b -> 'c -> int |}]
;;

let%expect_test _ =
  print_prog_result [((ELet (true, "series",
        (EFun ((PVar "n"),
           (EIf ((EBinOp (Eq, (EVar "n"), (EConst (CInt 1)))), (EConst (CInt 1)),
              (EBinOp (Add, (EVar "n"),
                 (EApp ((EVar "series"),
                    (EBinOp (Sub, (EVar "n"), (EConst (CInt 1))))))
                 ))
              ))
           ))
        )))];
  [%expect {| series -> int -> int |}]
;;
