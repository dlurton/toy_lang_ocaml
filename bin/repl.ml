
open Toy_lang.Types
open Toy_lang.Core
open Toy_lang.Pretty

let prompt = "> "

let pointer_offset = (String.length prompt - 1)

let rec user_input cb =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    cb v;
    user_input cb

let show_error sloc msg addl_pointer_offset =
  if (String.length sloc.file) <> 0 then Printf.printf "%s: " sloc.file
  else
    let pointer = (String.make (sloc.char_offset + pointer_offset + addl_pointer_offset) ' ') ^ "^" in
    print_endline pointer;
    Printf.printf "Error at char offset %d: %s\n" sloc.char_offset msg;
    flush(stdout)

let showAST e =
  try
    let resolved_e = (e |> Toy_lang.Resolve.resolve_rewrite |> pretty_string_of_expr) in
    Printf.printf "AST          : %s\n" (pretty_string_of_expr e);
    Printf.printf "Resolved AST : %s\n" resolved_e
  with InterpExn(loc, msg) -> show_error loc (string_of_error msg) 1

let interpret_line line_text =
  if String.length line_text >= 0 then
    let (onlyAST, line_text) = match String.get line_text 0 with
      | '?' -> (true, (String.sub line_text 1 ((String.length line_text) - 1)))
      | _ -> (false, line_text)
    in
    (* attempt to parse the line *)
    let presult = parse(line_text) in
    match presult with
    | PR_error(sloc, pmsg) -> show_error sloc pmsg 0 ;
    | PR_success e ->
      if onlyAST then
        showAST e
      else
        begin
          (* attempt to interpret the AST *)
          let iresult = eval_with_empty_env e in
          match iresult with
          | IR_error(sloc, err) ->
            show_error sloc (string_of_error err) 0;
          | IR_success value ->
            Printf.printf "Value: %s\n" (pretty_string_of_value(value));
        end

let () =
  let history_file = String.concat "" [(Unix.getenv "HOME"); "/.toy_lang.history"] in
  (* LNoise.set_multiline true; *)
  LNoise.history_load ~filename:history_file |> ignore;
  LNoise.history_set ~max_length:1000 |> ignore;
  (* Linenoise has a completion callback that we can use if we ever feel like
      supporting autocompletion: LNoise.set_completion_callback *)
  print_endline("OCaml toy language REPL - type \"exit\" to exit");
  (fun source_line ->
     if source_line = "exit" then exit 0;
     LNoise.history_add source_line |> ignore;
     LNoise.history_save ~filename:history_file |> ignore;
     interpret_line source_line;
     flush stdout
  )
  |> user_input
