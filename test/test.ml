open Toy_lang.Types
open Toy_lang.Core
open OUnit2;;

let test_eval (s: string) : value_t =
  let presult = parse(s) in
  match presult with
  | PR_error(_, pmsg) -> failwith("Parse error: " ^ pmsg)
  | PR_success e ->
    let iresult = eval_with_empty_env e in
    match iresult with
    | IR_error (_, imsg) -> failwith("Interp error: " ^ imsg)
    | IR_success r -> r

let value_to_int = function
   | VAL_i32 i -> i
   | _ -> failwith "Result of the function was not an int"

let value_to_bool = function
   | VAL_bool b -> b
   | _ -> failwith "Result of the function was not a bool"

let eval_int src = value_to_int (test_eval src)
let eval_bool src = value_to_bool (test_eval src)

let expect_int expected_result source =
  (fun _ ->
      let result = eval_int source in
      assert_equal expected_result result)
(* TODO: DRY *)
let expect_bool expected_result source =
  (fun _ ->
      let result = eval_bool source in
      assert_equal expected_result result)


let suite = "toy_lang_suite" >:::
            [
              (* literals *)
              "lit_int">::expect_int 22 "22";
              (* TODO:  full range of integers here, i.e MIN_INT / MAX_INT, etc *)
              "lit_bool_true">::expect_bool true "true";
              "lit_bool_false">::expect_bool false "false";
              (* artihmetic *)
              "binary_add_1">::expect_int 10 "8 + 2";
              "binary_add_2">::expect_int 196 "97 + 99";
              "binary_add_with_parens">::expect_int 22 "(10 + 1) + (5 + 6)";

              (* if *)
              "if_1">::expect_int 1 "if true then 1 else 2";
              "if_2">::expect_int 2 "if false then 1 else 2";

              (* let *)
              "let_1">::expect_int 99 "let x = 99 in x";
              "let_shadow_1">::expect_int 101 "let x = 99 in let x = 101 in x";
              "let_shadow_2">::expect_int 102 "let x = 99 in let x = 101 in x + 1";

              (* func *)
              "func_1">::expect_int 22 "(func(f) { f + 1 })(21)";
              "func_variable">::expect_int 22 "let p = func(f) { f + 1 } in p(21)";
              "func_">::expect_int 22 "(func(f) { f(21) })(func(x) { x + 1 })";
              "func_">::expect_int 22 "(func(x) { func(y) { x + y } })(10)(12)";
              "func_">::expect_int 22 "let f = func(x) { func(y) { x + y } } in f(10)(12)";

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

