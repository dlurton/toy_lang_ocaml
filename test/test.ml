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

              (* integer comparison *)
              "binary_gt_int_2_1">::expect_bool true "2 > 1";
              "binary_gt_int_2_2">::expect_bool false "2 > 2";
              "binary_gt_int_2_3">::expect_bool false "2 > 3";
              "binary_gte_int_2_1">::expect_bool true "2 >= 1";
              "binary_gte_int_2_2">::expect_bool true "2 >= 2";
              "binary_gte_int_2_3">::expect_bool false "2 >= 3";

              "binary_lt_int_2_1">::expect_bool false "2 < 1";
              "binary_lt_int_2_2">::expect_bool false "2 < 2";
              "binary_lt_int_2_3">::expect_bool true "2 < 3";
              "binary_lte_int_2_1">::expect_bool false "2 <= 1";
              "binary_lte_int_2_2">::expect_bool true "2 <= 2";
              "binary_lte_int_2_3">::expect_bool true "2 <= 3";

              (* integer comparison errors *)
              "binary_eq_mixed_1">::expect_error 1 3 (ERR_type_mismatch(TY_int, TY_bool)) "1 = true";
              "binary_eq_mixed_2">::expect_error 1 6 (ERR_type_mismatch(TY_bool, TY_int)) "true = 1";
              (* TODO: should function equality be supported or should it just be a error?
                 currently, all attempts to check equality of variables referencing functions
                 result in false.  This should probably be an error until we support it
                 properly. *)

              (* logical operators *)
              "and_1">::expect_bool true "true && true";
              "and_2">::expect_bool false "false && true";
              "and_3">::expect_bool false "true && false";
              "and_4">::expect_bool false "false && false";

              "or_1">::expect_bool true "true || true";
              "or_2">::expect_bool true "false || true";
              "or_3">::expect_bool true "true || false";
              "or_4">::expect_bool false "false && false";

              (* logical operator errors *)
              "and_not_bool_1">::expect_error 1 1 (ERR_expected_bool(TY_int)) "1 && true";
              "and_not_bool_2">::expect_error 1 9 (ERR_expected_bool(TY_int)) "true && 1";

              "or_not_bool_1">::expect_error 1 1  (ERR_expected_bool(TY_int)) "1 || true";
              "or_not_bool_2">::expect_error 1 10 (ERR_expected_bool(TY_int)) "false || 1";

              (* integer arithmetic *)
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
              "binary_div_by_zero">::expect_error 1 3 ERR_div_0 "1 / 0";

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
              "binary_mod_by_zero">::expect_error 1 3 ERR_div_0 "1 % 0";

              (* basic binary order of operations *)
              "binary_order_of_operations_1">::expect_int 7 "1 + 2 * 3";       (* * higher than + *)
              "binary_order_of_operations_2">::expect_int 9 "5 + 8 / 2";       (* / higher than + *)
              "binary_order_of_operations_3">::expect_int 4 "2 + 8 % 3";       (* % higher than + *)
              "binary_order_of_operations_4">::expect_int 20 "5 * 8 / 2";      (* / same as * *)
              "binary_order_of_operations_5">::expect_int 21 "14 / 2 * 3";     (* / same as * *)
              "binary_order_of_operations_6">::expect_int 1 "14 / 2 * 3 % 5";  (* % same as * and / *)
              "binary_order_of_operations_7">::expect_int 4 "14 * 2 % 10 / 2"; (* % same as * and / *)

              (* arithmetic errors *)
              "binary_add_int_bool">::expect_error 1 5 (ERR_expected_int(TY_bool)) "1 + true";
              "binary_add_bool_int">::expect_error 1 1 (ERR_expected_int(TY_bool)) "true + 1";
              "binary_add_bool_bool">::expect_error 1 1 (ERR_expected_int(TY_bool)) "true + true";

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
              "if_cond_not_bool">::expect_error 1 4 (ERR_expected_bool(TY_int)) "if 1 then 1 else 2";

              (* let *)
              "let_1">::expect_int 99 "let x:int = 99 in x";
              "let_shadow_1">::expect_int 101 "let x:int = 99 in let x:int = 101 in x";
              "let_shadow_2">::expect_int 102 "let x:int = 99 in let x:int = 101 in x + 1";

              (* func *)
              "func_zero_arg">::expect_int 11 "(func(-> int) -> 11)()";
              "func_single_arg">::expect_int 1 "(func(x:int -> int) -> x)(1)";
              "func_two_args_1">::expect_int 1 "(func(x:int, y:int -> int) -> x)(1, 2)";
              "func_two_args_2">::expect_int 2 "(func (x:int, y:int -> int) -> y)(1, 2)";
              "func_three_args_1">::expect_int 1 "(func(x:int, y:int, z:int -> int) -> x)(1, 2, 3)";
              "func_three_args_2">::expect_int 2 "(func (x:int, y:int, z:int -> int) -> y)(1, 2, 3)";
              "func_three_args_3">::expect_int 3 "(func (x:int, y:int, z:int -> int) -> z)(1, 2, 3)";
              "func_with_exp_1">::expect_int 22 "(func (x:int -> int) -> x + 1)(21)";
              "func_with_exp_2">::expect_int 32 "(func (x:int, y:int -> int) -> x + y)(10 ,22)";
              "func_with_exp_3">::expect_int 13 "(func (x:int, y:int, z:int -> int) -> x + y * z)(3, 2, 5)";
              "func_dup_arg_names">::expect_int 3 "(func (x:int, x:int -> int) -> x)(3, 2)";
              "func_as_arg">::expect_int 22 "(func(f:(int -> int) -> int) -> f(21))(func(x:int -> int) -> x + 1)";
              "func_returned">::expect_int 22 "(func(x:int -> (int -> int)) -> func(y:int -> int) -> x + y)(10)(12)";
              "func_in_let">::expect_int 22 "let f:(int -> (int -> int)) = func(x:int -> (int -> int)) -> func(y:int -> int) -> x + y in f(10)(12)";

              (* func errors *)
              "func_call_non_func">::expect_error 1 1 ERR_cannot_call_non_func "1(1)";
              "func_call_wrong_num_args_0_1">::expect_error
                1 22 (ERR_incorrect_arg_count(0, 1)) "(func ( -> int) -> 1)(1)";
              "func_call_wrong_num_args_2_0">::expect_error
                1 34 (ERR_incorrect_arg_count(2, 0)) "(func (x:int, y:int -> int) -> 1)()";
              "func_call_wrong_num_args_2_3">::expect_error
                1 34 (ERR_incorrect_arg_count(2, 3)) "(func (x:int, y:int -> int) -> 1)(1, 2, 3)";


              (* the y-combinator, just because *)
              (* TODO, here's an example: https://rosettacode.org/wiki/Y_combinator#OCaml *)
 
              (* let rec *)
              "let_rec_nested_func">::expect_int 100 "
let f:(int -> int) = func(factor:int -> int) ->
  let rec sum:(int -> int) = func(n:int -> int) ->
    if n = 0 then 0 else 10 + sum(n - 1)
  in sum(factor)
in f(10)
";
              "let_rec_nested_func_2">::expect_int 50 "
let f: (int, int -> int) =
  func(mutiple:int, factor: int -> int) ->
    let rec sum: (int -> int) = func(n:int -> int) ->
      if n = 0 then 0 else mutiple + sum(n - 1)
      in sum(factor)
  in
  f(5, 10)
";

              "let_rec_factorial">::expect_int 40320 "
let rec fact: (int -> int) = func(n:int -> int) ->
  if n = 0 then 1 else n * fact(n - 1)
in fact(8)
";

              "let_rec_fib">::expect_int 34 "
let rec fib: (int -> int) =
  func(n:int -> int) ->
    if n <= 1 then 1
    else fib(n - 1) + fib(n - 2)
  in
    fib(8)
";

              (* let rec ... and *)
              "let_rec_and_1">::expect_int 4 "
let rec f1: (int -> int) = func(n:int -> int) -> f2(n + 1)
and f2: (int -> int) = func (n:int -> int) -> n * 2
in f1(1) ";

              (* https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences *)
              "let_rec_and_hofstadter">::expect_int 12 "
let rec f: (int -> int) = func(n:int -> int) ->
  if n = 0 then 1
  else n - m(f(n - 1))
and m: (int -> int) = func(n: int -> int) ->
  if n = 0 then 0
  else n - f(m(n - 1))
in m(20)
";
           ]


let () = run_test_tt_main suite

;;

