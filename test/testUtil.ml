open Toy_lang.Types
open Toy_lang.Core
open Toy_lang.Rewrite
open Toy_lang.Pretty
open OUnit2;;

let test_parse (source: string) : expr_node_t =
  let presult = (parse source) in
  match presult with
  | PR_error(loc, msg) -> assert_failure (Printf.sprintf "Parse error: %s %s"
                                           (string_of_src_loc loc) msg)
  | PR_success e -> e 

let test_eval (s: string) : value_t =
  let exp = test_parse s in
  (* We are throwing in an additional rewrite pass here to ensure that the default_rewrite
     produces an exact clone of its original. *)
  let new_exp = default_rewrite exp in
  assert_bool "default_rewrite must return a new instance" (exp != new_exp);
  assert_equal exp new_exp
    ~printer:pretty_string_of_expr
    ~msg: "default_rewrite_must return an exact copy of the original";
  let iresult = eval_with_empty_env exp in
  match iresult with
  | IR_error (loc, err) -> assert_failure (Printf.sprintf "Interp error: %s %s"
                                           (string_of_src_loc loc)
                                           (string_of_error err))
  | IR_success r -> r

let value_to_int = function
   | VAL_int i -> i
   | _ -> assert_failure "Result of the function was not an int"

let value_to_bool = function
   | VAL_bool b -> b
   | _ -> assert_failure "Result of the function was not a bool"

let value_to_type = function
  | VAL_type t -> t
   | _ -> assert_failure "Result of the function was not a type"

let eval_int src = value_to_int (test_eval src)
let eval_bool src = value_to_bool (test_eval src)
let eval_type src = value_to_type (test_eval src)

let expect_int expected_result source =
  (fun _ ->
      let result = eval_int source in
      assert_equal expected_result result ~printer:string_of_int)

let expect_bool expected_result source =
  (fun _ ->
      let result = eval_bool source in
      assert_equal expected_result result ~printer:string_of_bool)

(*
let expect_type expected_type source =
  (fun _ ->
     let result = eval_type source in
     assert_equal expected_type = result)
*)

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

