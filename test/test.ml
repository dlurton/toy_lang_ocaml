open Toy_lang.Types
open Toy_lang.Core
open OUnit2;;

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


let make_test expected_result source =
  (fun _ ->
      let result = test_int source in
      assert_equal expected_result result)

let suite = "toy_lang_suite" >:::
            [
              (* literal integers *)
              "lit_int">::make_test 22 "22";
              (* TODO:  full range of integers here, i.e MIN_INT / MAX_INT, etc *)

              (* artihmetic *)
              "binary_add_1">::make_test 10 "8 + 2";
              "binary_add_2">::make_test 196 "97 + 99";
              "binary_add_with_parens">::make_test 22 "(10 + 1) + (5 + 6)";

              (* let *)
              "let_1">::make_test 99 "let x = 99 in x";
              "let_shadow_1">::make_test 101 "let x = 99 in let x = 101 in x";
              "let_shadow_2">::make_test 102 "let x = 99 in let x = 101 in x + 1";

              (* func *)
              "func_1">::make_test 22 "(func(f) { f + 1 })(21)";
              "func_variable">::make_test 22 "let p = func(f) { f + 1 } in p(21)";
              "func_">::make_test 22 "(func(f) { f(21) })(func(x) { x + 1 })";
              "func_">::make_test 22 "(func(x) { func(y) { x + y } })(10)(12)";
              "func_">::make_test 22 "let f = func(x) { func(y) { x + y } } in f(10)(12)";

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
            ]


let () = run_test_tt_main suite

;;

