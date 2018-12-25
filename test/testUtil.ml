open Toy_lang.Types
open Toy_lang.Core
open Toy_lang.Env
open Toy_lang.Rewrite
open Toy_lang.Pretty

open OUnit2;;

let test_parse (source: string) : expr_t =
  let presult = parse source in
  match presult with
  | PR_error(_, pmsg) -> assert_failure ("Parse error: " ^ pmsg)
  | PR_success e ->
    (* Include an additional assertion for default_rewrite here. This is a good
       place since every test calls this function and as a result we will
       also be testing default_rewrite_expr for free. *)
    let e_copy = default_rewrite_expr e in
    assert_equal e e_copy
      ~msg: "Default rewiter must produce an exact copy of the original"
      ~printer: pretty_string_of_expr;
    e


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
      assert_equal expected_result result ~printer:string_of_int)
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
       assert_equal expected_error err ~msg:"Error must match the expected error" ~printer:string_of_error;
       let { line_num; char_offset; _ } = src_loc in
       assert_equal expected_line_num line_num ~msg:"Error must originate at correct line" ~printer:string_of_int;
       assert_equal expected_char_offset char_offset ~msg:"Error must originate at correct character offset" ~printer:string_of_int)

