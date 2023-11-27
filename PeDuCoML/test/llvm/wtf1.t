Copyright 2023-2024, Kakadu and contributors
A test for miniML designed by Kakadu 

  $ cat > input.ml <<- EOF
  > let main = (fun x -> x) (fun x -> x) print_int 42
  > EOF

  $ cat input.ml | ./llvm_test.exe | lli-16 -load ../../runtime/peducoml_runtime.so
  Fatal error: exception Sexplib0__Sexp.Not_found_s(_)
  Raised at Base__Map.Tree0.find_exn.if_not_found in file "src/map.ml", line 503, characters 6-84
  Called from PeDuCoML__Llvm_comp.gather_args_numbers.count_args.process_cexpr in file "lib/llvm/llvm_comp.ml", line 304, characters 24-81
  Called from PeDuCoML__Llvm_comp.gather_args_numbers.count_args.process_aexpr in file "lib/llvm/llvm_comp.ml", line 351, characters 28-63
  Called from PeDuCoML__Llvm_comp.gather_args_numbers.count_args in file "lib/llvm/llvm_comp.ml", line 354, characters 4-46
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Base__List0.fold in file "src/list0.ml" (inlined), line 21, characters 22-52
  Called from PeDuCoML__Llvm_comp.gather_args_numbers in file "lib/llvm/llvm_comp.ml", line 357, characters 4-106
  Called from Dune__exe__Llvm_test.print_llvm in file "test/llvm/llvm_test.ml", line 27, characters 14-25
  lli-16: Symbols not found: [ main ]
  [1]
 
