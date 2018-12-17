
open Types
open Core
open Printf
open Pretty


;;

(* program entry point *)
print_endline("OCAML toy language REPL - type \"?exit\" to exit");

let show_error sloc msg =
  if (String.length sloc.file) != 0 then printf "%s: " sloc.file;
  printf "(%d, %d): %s\n" sloc.line_num sloc.char_offset msg;
  flush(stdout)
in

let line_reader = fun () -> try Some (input_line stdin) with End_of_file -> None in
let rec interpret_line () =
  print_string(">");
  flush(stdout);
  begin
    match line_reader() with
    | None -> exit(0)
    | Some line_text ->
      match line_text with
      | "?exit" -> exit(0)
      | _ ->
        (* attempt to parse the line *)
        let presult = parse(line_text) in
        match presult with
        | ParseError(sloc, pmsg) -> show_error sloc pmsg
        | ParseSuccess e ->
          printf "AST: %s\n" (pretty_string_of_expr e);
          (* attempt to interpret the AST *)
          let iresult = eval_with_empty_env e in
          match iresult with
          | InterpError(sloc, imsg) -> show_error sloc imsg
          | InterpSuccess value ->
            printf "Value: %s\n" (pretty_string_of_value value);
            ()
  end;
  interpret_line()
in
  interpret_line();
