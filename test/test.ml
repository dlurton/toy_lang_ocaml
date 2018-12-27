open Toy_lang.Types
open TestUtil
open OUnit2;;

let suite = "toy_lang_suite" >:::
            [
              (* literal bools *)
              "lit_bool_true">::expect_bool true "true";
              "lit_bool_false">::expect_bool false "false";

              (* literal ints *)
              "lit_int_01">::expect_int 0 "0";
              "lit_int_02">::expect_int 0 "-0";
              "lit_int_03">::expect_int 1 "1";
              "lit_int_04">::expect_int (-1) "-1";
              "lit_int_05">::expect_int 2 "2";
              "lit_int_06">::expect_int (-2) "-2";
              "lit_int_07">::expect_int 10 "10";
              "lit_int_08">::expect_int (-10) "-10";
              "lit_int_09">::expect_int 11 "11";
              "lit_int_10">::expect_int (-11) "-11";
              "lit_int_11">::expect_int 2147483647 "2147483647";
              "lit_int_12">::expect_int (-2147483647) "-2147483647";
              (* TODO: literal values that overflow and underflow 32-bit integers *)

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

              (* mixed type equality (TODO: should these really be errors instead of evaluating to false?) *)
              "binary_eq_mixed_1">::expect_bool false "1 = true";
              "binary_eq_mixed_2">::expect_bool false "true = 1";
              (* TODO: should function equality be supported or should it just be a error? *)

              "binary_add_1">::expect_int 10 "8 + 2";
              "binary_add_2">::expect_int 196 "97 + 99";
              "binary_add_3">::expect_int 22 "8 + 2 + 12";
              "binary_add_4">::expect_int 22 "(10 + 1) + (5 + 6)";

              (* TODO: positive and negative literals and results *)
              "binary_sub_1">::expect_int 6 "8 - 2";
              "binary_sub_2">::expect_int (-2) "97 - 99";
              "binary_sub_3">::expect_int (-6) "8 - 2 - 12";
              "binary_sub_4">::expect_int 10 "(10 - 1) - (5 - 6)";

              "binary_mul_01">::expect_int 0 "1 * 0";
              "binary_mul_02">::expect_int 0 "0 * 1";
              "binary_mul_03">::expect_int 1 "1 * 1";
              "binary_mul_04">::expect_int 0 "-1 * 0";
              "binary_mul_05">::expect_int 0 "0 * -1";
              "binary_mul_06">::expect_int (-1) "1 * -1";
              "binary_mul_07">::expect_int (-1) "-1 * 1";
              "binary_mul_08">::expect_int 4 "2 * 2";
              "binary_mul_09">::expect_int 8 "2 * 2 * 2";
              "binary_mul_10">::expect_int 16 "2 * 2 * 2 * 2";
              "binary_mul_11">::expect_int 32 "2 * 2 * 2 * 2 * 2";
              "binary_mul_12">::expect_int 64 "2 * 2 * 2 * 2 * 2 * 2";
              "binary_mul_13">::expect_int 128 "2 * 2 * 2 * 2 * 2 * 2 * 2";
              "binary_mul_14">::expect_int 256 "2 * 2 * 2 * 2 * 2 * 2 * 2 * 2";

              "binary_div_01">::expect_int 0 "0 / 1";
              "binary_div_02">::expect_int 0 "0 / -1";
              "binary_div_03">::expect_int 0 "1 / 2";
              "binary_div_04">::expect_int 0 "1 / -2";
              "binary_div_05">::expect_int 0 "-1 / 2";
              "binary_div_06">::expect_int 0 "-1 / -2";
              "binary_div_07">::expect_int 1 "1 / 1";
              "binary_div_08">::expect_int (-1) "1 / -1";
              "binary_div_09">::expect_int (-1) "-1 / 1";
              "binary_div_10">::expect_int (-1) "-1 / 1";
              "binary_div_11">::expect_int 1 "2 / 2";
              "binary_div_12">::expect_int 2 "4 / 2";
              "binary_div_13">::expect_int 4 "8 / 2";
              "binary_div_14">::expect_int 32768 "65536 / 2";
              (*TODO divide by 0 (don't have this kind of error handling wired up yet) *)

              "binary_mod_01">::expect_int 0 "0 % 1";
              "binary_mod_02">::expect_int 0 "0 % -1";
              "binary_mod_03">::expect_int 1 "1 % 2";
              "binary_mod_04">::expect_int 1 "1 % -2";
              "binary_mod_05">::expect_int (-1) "-1 % 2";
              "binary_mod_06">::expect_int (-1) "-1 % -2";
              "binary_mod_07">::expect_int 4 "4 % 5";
              "binary_mod_08">::expect_int 3 "3 % 5";
              "binary_mod_09">::expect_int 2 "2 % 5";
              "binary_mod_10">::expect_int 1 "1 % 5";
              (*TODO mod by 0 *)

              (* basic binary order of operations *)
              "binary_order_of_operations_1">::expect_int 7 "1 + 2 * 3";       (* * higher than + *)
              "binary_order_of_operations_2">::expect_int 9 "5 + 8 / 2";       (* / higher than + *)
              "binary_order_of_operations_3">::expect_int 4 "2 + 8 % 3";       (* % higher than + *)
              "binary_order_of_operations_4">::expect_int 20 "5 * 8 / 2";      (* / same as * *)
              "binary_order_of_operations_5">::expect_int 21 "14 / 2 * 3";     (* / same as * *)
              "binary_order_of_operations_6">::expect_int 1 "14 / 2 * 3 % 5";  (* % same as * and / *)
              "binary_order_of_operations_7">::expect_int 4 "14 * 2 % 10 / 2"; (* % same as * and / *)

              (* arithmetic errors *)
              "binary_add_int_bool">::expect_error 1 3 ERR_arithmetic_on_non_number "1 + true";
              "binary_add_bool_int">::expect_error 1 6 ERR_arithmetic_on_non_number "true + 1";
              "binary_add_bool_bool">::expect_error 1 6 ERR_arithmetic_on_non_number "true + true";

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

              (* let rec *)
              "let_rec_factorial">::expect_int 40320 "let rec fact = func n -> if n = 0 then 1 else n * fact(n - 1) in fact(8)";

              "let_rec_fib">::expect_int 34 {str|
let rec fib =
  func n ->
    // TODO:  when <= is added, change this nested if else to if n <= 1
    if n = 0 then 1
    else if n = 1 then 1
    else fib(n - 1) + fib(n - 2)
  in
    fib(8)
|str};
              "let_rec_nested">::expect_int 00 {str|
let f =
  func mutiple factor ->
    let rec sum = func n ->
      if n = 0 then 0 else mutiple + sum(n - 1)
      in sum(factor)
  in
  f(5 10)
|str};
              (* func *)
              "func_1">::expect_int 22 "(func f -> f + 1)(21)";
              "func_2">::expect_int 32 "(func x y -> x + y)(10 22)";
              "func_3">::expect_int 17 "(func x y z -> x + y * z)(3 2 5)";
              "func_variable">::expect_int 22 "let p = func f -> f + 1 in p(21)";
              "func_as_arg">::expect_int 22 "(func f -> f(21))(func x -> x + 1)";
              "func_returned">::expect_int 22 "(func x -> func y -> x + y)(10)(12)";
              "func_in_let">::expect_int 22 "let f = func x -> func y -> x + y in f(10)(12)";

              (* func errors *)
              "func_call_non_func">::expect_error 1 1 ERR_invoked_non_func "1(1)";

              (* the y-combinator, just because *)
              (* TODO, here's an example: https://rosettacode.org/wiki/Y_combinator#OCaml *)
            ]


let () = run_test_tt_main suite

;;

