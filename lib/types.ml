(*
   This file contain the core data type definitions.
*)

(* The evaluation-time "ground" types. *)
type src_loc_t = {
  file: string;
  line_num: int;
  char_offset: int;
}

let string_of_src_loc (sloc:src_loc_t) =
  Printf.sprintf "%s:%d:%d" sloc.file sloc.line_num sloc.char_offset

let make_src_loc file line_num char_offset = {
  file = file;
  line_num = line_num;
  char_offset = char_offset
}

let dummy_src_loc = {
  file = "TODO";
  line_num = -1;
  char_offset = -1;
}

type op_t =
  | OP_add
  | OP_sub
  | OP_mul
  | OP_div
  | OP_mod
  | OP_eq
  | OP_gt
  | OP_gte
  | OP_lt
  | OP_lte

type logical_op_t =
  | LOP_and (* logical and *)
  | LOP_or (* logical or *)

(* The types of the language. *)
and value_t =
  | VAL_bool of bool
  | VAL_i32 of int
  (* tuple fields: argument count, function body, captured environment *)
  | VAL_func of int * expr_t * env_t
(* An environment is, for now, simply a list of value_t *)
and env_t = value_t array list
(* The AST. *)
and expr_t = {
  exp : expr_node_t;
  loc : src_loc_t;
}
and expr_node_t =
  | EXPN_var      of string
  | EXPN_index    of int * int
  | EXPN_literal  of value_t
  | EXPN_binary   of op_t * expr_t * expr_t
  | EXPN_logical  of logical_op_t * expr_t * expr_t
  | EXPN_let      of string * expr_t * expr_t
  | EXPN_let_rec  of var_def_t list * expr_t
  | EXPN_if       of expr_t * expr_t * expr_t
  | EXPN_func     of string list * expr_t
  | EXPN_call     of expr_t * (expr_t list)
and var_def_t = string * expr_t

(* TODO: parse_result and interp_result really should have the _t suffix. *)
(* The result of an attempt to parse a snippet of code. *)
type parse_result =
    PR_error of src_loc_t * string
  | PR_success of expr_t

(* TODO: Consider splitting this variant type up into semantic_error_t and eval_error_t. *)
type error_t =
  | ERR_unbound_var of string (* can only happen during the resolve rewrite. *)
  | ERR_if_cond_not_bool
  | ERR_logical_operand_not_bool
  | ERR_invoked_non_func
  | ERR_arithmetic_on_non_number
  | ERR_div_0
  | ERR_incorrect_arg_count of int * int

let string_of_error err =
  match err with
  | ERR_unbound_var id -> Printf.sprintf "unbound variable '%s'" id
  | ERR_if_cond_not_bool -> "if condition was not a bool value"
  | ERR_logical_operand_not_bool -> "operand to logical operator was not a boolean value"
  | ERR_invoked_non_func -> "cannot call a value that is not a function"
  | ERR_div_0 -> "divide by zero"
  | ERR_arithmetic_on_non_number -> "attempted to perform arithmetic on a value that was not a number"
  | ERR_incorrect_arg_count(expected, actual) ->
    Printf.sprintf "function expected %d argument(s) but %d were supplied" expected actual

(* The result of an attempt to interpret an AST. *)
type interp_result =
    IR_error of src_loc_t * error_t
  | IR_success of value_t

(* Exception thrown by the lexer when an error is encountered. *)
exception LexicalExn of src_loc_t * string

(* Exception thrown by the interpreter when an error is encountered. *)
exception InterpExn of src_loc_t * error_t

