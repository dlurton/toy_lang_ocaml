
open Types
open Core;;

(* program entry point *)
print_endline("OCAML toy language REPL - type \"?exit\" to exit");

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
        | ParseError pmsg -> print_endline("Parse error: " ^ pmsg)
        | ParseSuccess e ->
          (* attempt to interpret the AST *)
          let iresult = eval_with_empty_env e in
          match iresult with
          | InterpError imsg -> print_endline("Interp error: " ^ imsg)
          | InterpSuccess value ->
            print_endline(value_to_string(value))
  end;
  interpret_line()
in
  interpret_line();
