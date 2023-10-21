open PeDuCoML.Parser
open PeDuCoML.Inferencer
open PeDuCoML.Typing
open PeDuCoML.Pprintanf
open PeDuCoML.Anf

(* Doesn't work yet, this is a template for the future *)
let print_anf code =
  match parse code with
  | Ok ast ->
    (match R.run (check_types ast) with
     | Ok _ ->
       let anf = run_convert ast in
       List.iter (fun anf_node -> Format.printf "%a\n;;\n" pp_aexpr anf_node) anf
     | Error err -> print_type_error err)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  print_anf code
;;
