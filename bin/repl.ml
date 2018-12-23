
open Toy_lang.Types
open Toy_lang.Core
open Toy_lang.Pretty

;;

let rec user_input prompt cb =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    cb v;
    user_input prompt cb

let show_error sloc msg =
  if (String.length sloc.file) != 0 then Printf.printf "%s: " sloc.file;
  Printf.printf "(%d, %d): %s\n" sloc.line_num sloc.char_offset msg;
  flush(stdout)

let interpret_line line_text =
  (* attempt to parse the line *)
  let presult = parse(line_text) in
  match presult with
  | PR_error(sloc, pmsg) -> show_error sloc pmsg
  | PR_success e ->
    (* attempt to interpret the AST *)
    let iresult = eval_with_empty_env e in
    match iresult with
    | IR_error(sloc, err) -> show_error sloc (string_of_error err)
    | IR_success value ->
      print_endline(pretty_string_of_value(value))

let () =
  let history_file = String.concat "" [(Unix.getenv "HOME"); "/.toy_lang.history"] in
  (* LNoise.set_multiline true; *)
  LNoise.history_load ~filename:history_file |> ignore;
  LNoise.history_set ~max_length:1000 |> ignore;
  (* TODO: LNoise.set_completion_callback *)
  print_endline("OCAML toy language REPL - type \"?exit\" to exit");
  (fun from_user ->
     if from_user = "?exit" then exit 0;
     LNoise.history_add from_user |> ignore;
     LNoise.history_save ~filename:history_file |> ignore;
     interpret_line from_user
  )
  |> user_input"> "
