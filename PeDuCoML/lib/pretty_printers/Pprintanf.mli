open Anf
open Format

val pp_id : formatter -> unique_id -> unit
val pp_immexpr : formatter -> imm_expr -> unit
val pp_cexpr : formatter -> cexpr -> unit
val pp_aexpr : formatter -> aexpr -> unit
val pp_global_scope_function : formatter -> tag * imm_expr list * aexpr -> unit
