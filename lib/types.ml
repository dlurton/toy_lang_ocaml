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

type type_t =
  | TY_int
  | TY_bool
  | TY_func of func_type_t

and func_type_t = type_t list * type_t

(* The types of the language. *)
and value_t =
  | VAL_bool of bool
  | VAL_int of int
  (* argument count * function body * captured environment *)
  | VAL_func of int * expr_node_t * env_t
  (* TODO:  remove VAL_TYPE *)
  | VAL_type of type_t

(* An environment is, for now, simply a list of value_t *)
and env_t = value_t array list

(* Contains an expression and  meta information about it expression
   such as a line & column informmaation. *)
and expr_node_t = {
  exp : expr_t;
  loc : src_loc_t;
}

(* Represents an expression.  Child nodes are represented with
   and instance of expr_node_t, which contains the child expression
   and its meta-information.  *)
and expr_t =
  (* variable id *)
  | EXP_var      of string
  (* environment offset * variable index * variable type *)
  | EXP_index    of int * int * type_t
  (* literal value *)
  | EXP_literal  of value_t
  (* operation * left operand * right operand *)
  | EXP_binary   of op_t * expr_node_t * expr_node_t
  (* operation * left operand * right operand *)
  | EXP_logical  of logical_op_t * expr_node_t * expr_node_t
  (* variable definition * let body *)
  | EXP_let      of var_def_t * expr_node_t
  (* list of variable definitions * let body *)
  | EXP_let_rec  of var_def_t list * expr_node_t
  (* condition * then expression * else expression *)
  | EXP_if       of expr_node_t * expr_node_t * expr_node_t
  (* argument definitions * return type * function body *)
  | EXP_func     of param_def_t list * type_t * expr_node_t
  (* function expression * argument list *)
  | EXP_call     of expr_node_t * (expr_node_t list)

(* variable name * variable type * value expression *)
and var_def_t = string * type_t * expr_node_t

(* parameter name * parameter type *)
and param_def_t = string * type_t

(* TODO: parse_result and interp_result really should have the _t suffix. *)
(* The result of an attempt to parse a snippet of code. *)
type parse_result =
    PR_error of src_loc_t * string
  | PR_success of expr_node_t

(* TODO: Consider splitting this variant type up into semantic_error_t and eval_error_t. *)
type error_t =
  | ERR_unbound_var of string (* can only happen during the resolve rewrite. *)
  | ERR_expected_bool of type_t
  | ERR_expected_int of type_t
  | ERR_type_mismatch of type_t * type_t 
  | ERR_if_cond_not_bool (* TODO:  remove this *)
  | ERR_logical_operand_not_bool (* TODO: remove this *)
  | ERR_if_branch_type_mismatch (* TODO: remove this *)
  | ERR_cannot_call_non_func
  | ERR_invoked_non_func (* TODO: remove *)
  | ERR_arithmetic_on_non_number
  | ERR_div_0
  | ERR_incorrect_arg_count of int * int
  | ERR_arg_type_mismatch of int * type_t * type_t

let string_of_error err =
  match err with
  | ERR_unbound_var id -> Printf.sprintf "unbound variable '%s'" id
  | ERR_expected_bool _ ->
    (* TODO: include the actual type in this error message--currently can't because of dependency cycle *)
    "Expected a bool value"
  | ERR_expected_int _ ->
    (* TODO: include the actual type in this error message--currently can't because of dependency cycle *)
    "Expected an int value"
  | ERR_type_mismatch (_, _) ->
    (* TODO: include the actual types in this error message--currently can't because of dependency cycle *)
    "Type mismatch"
  | ERR_if_cond_not_bool -> "if condition was not a bool value"
  | ERR_if_branch_type_mismatch -> "Both branches of if expression must have the same type"
  | ERR_logical_operand_not_bool -> "operand to logical operator was not a boolean value"
  | ERR_cannot_call_non_func -> "Value is not a function"
  | ERR_invoked_non_func -> "cannot call a value that is not a function"
  | ERR_div_0 -> "divide by zero"
  | ERR_arithmetic_on_non_number -> "attempted to perform arithmetic on a value that was not a number"
  | ERR_incorrect_arg_count(expected, actual) ->
    Printf.sprintf "function expected %d argument(s) but %d were supplied" expected actual
  | ERR_arg_type_mismatch (arg_num, _, _) ->
    (* TODO: include the actual types in this error message--currently can't because of dependency cycle *)
    (* TODO: include the actual types in this error message--currently can't because of dependency cycle *)
    Printf.sprintf "Argument %d has an incorrect type" arg_num


(* The result of an attempt to interpret an AST. *)
type interp_result =
    IR_error of src_loc_t * error_t
  | IR_success of value_t

(* Exception thrown by the lexer when an error is encountered. *)
exception LexicalExn of src_loc_t * string

(* Exception thrown by the interpreter when an error is encountered. *)
exception InterpExn of src_loc_t * error_t

