type riscv_error

val codegen : Anf.global_scope_function list -> (unit, riscv_error) Result.t
