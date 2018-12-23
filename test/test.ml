open Toy_lang.Types
open Toy_lang.Core
open OUnit2;;

let test_parse (source: string) : expr_t =
  let presult = parse source in
  match presult with
  | PR_error(_, pmsg) -> assert_failure ("Parse error: " ^ pmsg)
  | PR_success value -> value

let test_eval (s: string) : value_t =
  let exp = test_parse s in
  let iresult = eval_with_empty_env exp in
  match iresult with
  | IR_error (_, err) -> assert_failure ("Interp error: " ^ (string_of_error err))
  | IR_success r -> r

let value_to_int = function
   | VAL_i32 i -> i
   | _ -> assert_failure "Result of the function was not an int"

let value_to_bool = function
   | VAL_bool b -> b
   | _ -> assert_failure "Result of the function was not a bool"

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

let expect_error expected_line_num expected_char_offset expected_error source =
  (fun _ -> let exp = test_parse source in
     let iresult = eval exp empty_env in
     match iresult with
     | IR_success _ -> assert_failure "Expected an error but an error didn't occur."
     | IR_error (src_loc, err) ->
       assert_equal expected_error err ~msg:"Error must match the expected error" ~printer:(fun err -> string_of_error err);
       let { line_num; char_offset; _ } = src_loc in
       assert_equal expected_line_num line_num ~msg:"Error must originate at correct line" ~printer:string_of_int;
       assert_equal expected_char_offset char_offset ~msg:"Error must originate at correct character offset" ~printer:string_of_int)



let suite = "toy_lang_suite" >:::
            [
              (* literals *)
              "lit_int">::expect_int 22 "22";
              (* TODO:  full range of integers here, i.e MIN_INT / MAX_INT, etc *)
              "lit_bool_true">::expect_bool true "true";
              "lit_bool_false">::expect_bool false "false";

              (* variable errors *)
              "var_unbound">::expect_error 1 1 (ERR_unbound_var("some_unbound_var")) "some_unbound_var";

              (* artihmetic *)
              "binary_add_1">::expect_int 10 "8 + 2";
              "binary_add_2">::expect_int 196 "97 + 99";

              (* arithmetic errors *)
              "binary_add_with_parens">::expect_int 22 "(10 + 1) + (5 + 6)";
              "binary_add_int_bool">::expect_error 1 5 ERR_arithmetic_on_non_number "1 + true";
              "binary_add_bool_int">::expect_error 1 2 ERR_arithmetic_on_non_number " true + 1";
              "binary_add_bool_bool">::expect_error 1 2 ERR_arithmetic_on_non_number " true + true";

              (* if *)
              "if_1">::expect_int 1 "if true then 1 else 2";
              "if_2">::expect_int 2 "if false then 1 else 2";

              (* if errors *)
              "if_cond_not_bool">::expect_error 1 4 ERR_if_cond_not_bool "if 1 then 1 else 2";

              (* let *)
              "let_1">::expect_int 99 "let x = 99 in x";
              "let_shadow_1">::expect_int 101 "let x = 99 in let x = 101 in x";
              "let_shadow_2">::expect_int 102 "let x = 99 in let x = 101 in x + 1";

              (* func *)
              "func_1">::expect_int 22 "(func(f) { f + 1 })(21)";
              "func_variable">::expect_int 22 "let p = func(f) { f + 1 } in p(21)";
              "func_as_arg">::expect_int 22 "(func(f) { f(21) })(func(x) { x + 1 })";
              "func_returned">::expect_int 22 "(func(x) { func(y) { x + y } })(10)(12)";
              "func_in_let">::expect_int 22 "let f = func(x) { func(y) { x + y } } in f(10)(12)";

              (* func errors *)
              "func_call_non_func">::expect_error 1 1 ERR_invoked_non_func "1(1)"

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

