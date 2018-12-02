
open Ast
open Interp;;


let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None);

let main =
    print_string("OCAML toy language REPL - type \"?exit\" to exit"); 
    let rec eval_line line_stream =
      match Stream.next(line_stream) with
      | None -> ()
      | Some line_text ->
        (match line_text with
        | "?exit" -> ()
        | _ ->
          let value = interp(line_text) in
          let value_text = string_of_int(value) in
          print_string(value_text);
          eval_line(line_stream)
        )
    in let stdin_line_stream = line_stream_of_channel(stdin) in
           eval_line(stdin_line_stream)
