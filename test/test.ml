open Toy_lang.Types
open Toy_lang.Core;;


let value_to_int = function
   | VAL_i32 i -> i
   | VAL_func _ -> failwith "TODO"

let test_int (s: string) : int =
  let presult = parse(s) in
  match presult with
  | PR_error(_, pmsg) -> failwith("Parse error: " ^ pmsg)
  | PR_success e ->
    let iresult = eval_with_empty_env e in
    match iresult with
    | IR_error (_, imsg) -> failwith("Interp error: " ^ imsg)
    | IR_success r -> value_to_int(r)

(* A few test cases *)
let run_tests ()  =
  print_string "Running the tests...\n";
  (* basic expressions.  Only integers and + are currently supported. *)
  assert (22 = test_int "22");
  assert (22 = test_int "11+11");
  assert (22 = test_int "(10+1)+(5+6)");

  (* let *)
  assert (22 = test_int "let x = 22 in x");
  assert (22 = test_int "let x = 0 in let x = 22 in x");

  (* functions *)
  assert (22 = test_int "(func(f) f + 1)(21)");
  assert (22 = test_int "let p = func(f) f + 1 in p(21)");
  assert (22 = test_int "(func(f) f(21))(func(x) x + 1)");
  assert (22 = test_int "(func(x) func(y) x + y)(10)(12)");
  assert (22 = test_int "let f = func(x) func(y) x + y in f(10)(12)");

  (*Migrate the following test cases as well:
      
      (y-combinator-1 "
let fix =  proc (f) in
let d = proc (x) proc (z) ((f (x x)) z) in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
      ))
*)
  print_string "Done!\n";;

(* program entry point. *)
run_tests();;

