open PeDuCoML.Parser
open PeDuCoML.Inferencer
open PeDuCoML.Typing

(* Doesn't work yet, this is a template for the future *)
let print_anf code =
  match parse code with
  | Ok ast ->
    (match R.run (check_types ast) with
    | Ok typ_list -> 
      List.iter (fun (name, (_, typ)) -> Format.printf "%s: %a\n" name pp_type typ) typ_list
    | Error err -> print_type_error err)
  | Error err -> Format.printf "%s\n" err
;;

let _ =
  let code = Stdio.In_channel.input_all Caml.stdin in
  print_anf code
;;