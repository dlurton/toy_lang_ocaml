open Types
open Core;;

(* TODO: fold in to pattern matched eval function *)
let test_int (s: string) : int =
  let presult = parse(s) in
  match presult with
  | ParseError(_, pmsg) -> failwith("Parse error: " ^ pmsg)
  | ParseSuccess e ->
    let iresult = eval_with_empty_env e in
    match iresult with
    | InterpError (_, imsg) -> failwith("Interp error: " ^ imsg)
    | InterpSuccess r -> value_to_int(r)

(* A few test cases *)
let run_tests ()  =
  print_string "Running the tests...\n";
  assert (22 = test_int "22");
  assert (22 = test_int "11+11");
  assert (22 = test_int "(10+1)+(5+6)");
  assert (22 = test_int "let x = 22 in x");
  assert (22 = test_int "let x = 0 in let x = 22 in x");
  print_string "Done!\n";;

(* program entry point. *)
run_tests();;

