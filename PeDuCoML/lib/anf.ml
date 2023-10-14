open Ast

type immexpr =
  | ImmNum of int
  | ImmString of string
  | ImmId of int

type cexpr =
  | CBop of binary_operator * immexpr * immexpr
  | CImmExpr of immexpr

(* TODO: Perhaps id in ALet should be ImmId? *)
type aexpr =
  | ALet of int * cexpr * aexpr
  | ACExpr of cexpr

(* Smart constructors *)
let immnum num = ImmNum num
let immstring str = ImmString str
let immid id = ImmId id
let cbop bop left right = CBop (bop, left, right)
let cimmexpr immexpr = CImmExpr immexpr
let alet id cexpr aexpr = ALet (id, cexpr, aexpr)
let acexpr cexpr = ACExpr cexpr
let acimmexpr immexpr = acexpr @@ cimmexpr immexpr

module AnfConvert : sig
  type 'a t

  val fresh : int t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a
end = struct
  type 'a t = int -> 'a * int

  let fresh last = last + 1, last

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f state ->
    let value, st = m state in
    (f value) st
 ;;

  let bind m ~f = m >>= f
  let return : 'a -> 'a t = fun value last -> value, last
  let ( let* ) = ( >>= )
  let run monad = fst @@ monad 0

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun m f state ->
    let value, st = m state in
    f value, st
 ;;
end

open AnfConvert

let convert =
  let rec anf (expr : expression) (k : immexpr -> aexpr t) : aexpr AnfConvert.t =
    match expr with
    | ELiteral literal ->
      (match literal with
       | LInt num -> k (immnum num)
       | _ -> failwith "Not Implemented")
    | EIdentifier _ ->
      let* fresh = fresh in
      k (immid fresh)
    | EBinaryOperation (bop, left, right) ->
      let* fresh = fresh in
      anf left (fun limm ->
        anf right (fun rimm ->
          let* body = k @@ immid fresh in
          return @@ alet fresh (cbop bop limm rimm) body))
    | _ -> failwith "Not Implemented"
  in
  anf
;;

let run_convert expression = run @@ convert expression (fun imm -> return (acimmexpr imm))

let%test _ =
  run_convert @@ EBinaryOperation (Div, ELiteral (LInt 4), ELiteral (LInt 2))
  = ALet (1, CBop (Div, ImmNum 4, ImmNum 2), ACExpr (CImmExpr (ImmId 1)))
;;