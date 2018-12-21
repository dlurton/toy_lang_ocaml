
open Toy_lang.Types
open Toy_lang.Core
open Toy_lang.Pretty
;;

(* program entry point *)
print_endline("OCAML toy language REPL - type \"?exit\" to exit");

let show_error sloc msg =
  if (String.length sloc.file) != 0 then Printf.printf "%s: " sloc.file;
  Printf.printf "(%d, %d): %s\n" sloc.line_num sloc.char_offset msg;
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
        | PR_error(sloc, pmsg) -> show_error sloc pmsg
        | PR_success e ->
          (* attempt to interpret the AST *)
          let iresult = eval_with_empty_env e in
          match iresult with
          | IR_error(sloc, imsg) -> show_error sloc imsg
          | IR_success(value) ->
            print_endline(pretty_string_of_value value)
  end;
  interpret_line()
in
  interpret_line();
