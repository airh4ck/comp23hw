open Anf
open Llvm

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context
let i64 = i64_type context

let create_entry_block_alloca func var =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block func)) in
  Llvm.build_alloca i64 var builder
;;

open Result

let codegen =
  let string_of_unique_id = function
    | AnfId id -> string_of_int id
    | GlobalScopeId id -> id
  in
  (* : global_scope_function list -> llvalue list = *)
  let codegen_immexpr env = function
    | ImmInt num -> ok @@ const_int i64 num
    | ImmString str -> ok @@ const_string context str
    | ImmChar c -> ok @@ const_string context (String.make 1 c)
    | ImmBool v -> ok @@ const_int i64 (Bool.to_int v)
    | ImmId id ->
      (match Base.Map.find env id with
       | None -> error @@ `UnboundVariable id
       | Some llvalue -> ok @@ build_load2 i64 llvalue (string_of_unique_id id) builder)
    | _ -> 
      (* TODO: REMOVE UNIT MOTHERFUCKER *)
      failwith "AKLSJF:KLSJCKLSAJFK:LDS"
  in
  let codegen_cexpr env = function
  | 
;;
