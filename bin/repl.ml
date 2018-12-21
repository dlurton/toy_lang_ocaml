
open Toy_lang.Types
open Toy_lang.Core

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
  | ParseError(sloc, pmsg) -> show_error sloc pmsg
  | ParseSuccess e ->
    (* attempt to interpret the AST *)
    let iresult = eval_with_empty_env e in
    match iresult with
    | InterpError(sloc, imsg) -> show_error sloc imsg
    | InterpSuccess value ->
      print_endline(value_to_string(value))

let () =
  (* LNoise.set_multiline true; *)
  LNoise.history_load ~filename:"toy_lang.history" |> ignore;
  LNoise.history_set ~max_length:1000 |> ignore;
  LNoise.set_completion_callback begin fun line_so_far ln_completions ->
    if line_so_far <> "" && line_so_far.[0] = 'h' then
      ["if"; "func";]
      |> List.iter (LNoise.add_completion ln_completions);
  end;
  print_endline("OCAML toy language REPL - type \"?exit\" to exit");
  (fun from_user ->
     if from_user = "?exit" then exit 0;
     LNoise.history_add from_user |> ignore;
     LNoise.history_save ~filename:"history.txt" |> ignore;
     Printf.sprintf "Got: %s" from_user |> interpret_line
  )
  |> user_input "> "
