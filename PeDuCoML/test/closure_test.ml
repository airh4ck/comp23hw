open PeDuCoML.Parser
open PeDuCoML.Pprintast
open PeDuCoML.Closure_conversion

let print_anf code =
  match parse code with
  | Ok ast ->
    let closure = run_closure ast in
    List.iter (fun decl -> Format.printf "%a\n" pp_declaration decl) closure
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  print_anf code
;;
