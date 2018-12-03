
open Ast
open Interp;;

(* main entry point *)
print_endline("OCAML toy language REPL - type \"?exit\" to exit");
let line_reader = fun () -> try Some (input_line stdin) with End_of_file -> None in
let rec interpret_line () =
  print_string(">");
  flush(stdout);
  match line_reader() with
  | None -> ()
  | Some line_text ->
    match line_text with
    | "?exit" -> ()
    | _ ->
      let value = interp(line_text) in
      let value_text = string_of_int(value) in
      print_endline (value_text);
      interpret_line();
in
  interpret_line()
