open Toy_lang.Types
open TestUtil
open OUnit2;;

let suite = "toy_lang_suite" >:::
            [
              (* literals *)
              "lit_int">::expect_int 22 "22";
              (* TODO:  full range of integers here, i.e MIN_INT / MAX_INT, etc *)
              "lit_bool_true">::expect_bool true "true";
              "lit_bool_false">::expect_bool false "false";

              (* variable errors *)
              "var_unbound">::expect_error 1 1 (ERR_unbound_var("some_unbound_var")) "some_unbound_var";

              (* boolean equality *)
              "binary_eq_bool_1">::expect_bool true "true = true";
              "binary_eq_bool_2">::expect_bool true "false = false";
              "binary_eq_bool_3">::expect_bool false "true = false";
              "binary_eq_bool_4">::expect_bool false "false = true";

              (* integer equality *)
              "binary_eq_int_1">::expect_bool true "1 = 1";
              "binary_eq_int_2">::expect_bool true "10 = 10";
              "binary_eq_int_3">::expect_bool false "1 = 2";
              "binary_eq_int_4">::expect_bool false "2 = 1";
              "binary_eq_int_5">::expect_bool true "9 + 1 = 8 + 2";

              (* mixed type equality (TODO: should these really be errors?) *)
              "binary_eq_mixed_1">::expect_bool false "1 = true";
              "binary_eq_mixed_2">::expect_bool false "true = 1";

              (* should function equality be supported or should it just be a error? *)

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

              "if_exp_1">::expect_int 1 "if 1 = 1 then 1 else 2";
              "if_exp_2">::expect_int 2 "if 1 = 2 then 1 else 2";

              "if_nested_1">::expect_int 1 "if true then if true then 1 else 2 else -1";
              "if_nested_2">::expect_int 2 "if true then if false then 1 else 2 else -1";
              "if_nested_3">::expect_int 1 "if false then -1 else if true then 1 else 2";
              "if_nested_4">::expect_int 2 "if false then -1 else if false then 1 else 2";

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

