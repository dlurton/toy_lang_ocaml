(*
   This file contain the core data type definitions.
*)

(* The evaluation-time "ground" types. *)
type src_loc_t = {
  file: string;
  line_num: int;
  char_offset: int;
}

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

type value_t =
  | VAL_i32 of int 

(* The AST. *)
type expr_t = {
  exp : expr_node_t;
  loc : src_loc_t;
}
and expr_node_t =
  | EXPN_var of string
  | EXPN_literal of value_t
  | EXPN_add of expr_t * expr_t
  | EXPN_let of string * expr_t * expr_t
  (*| Proc of string * expr *)

let value_to_int = function
  | VAL_i32 i -> i

let value_to_string = function
  | VAL_i32 i -> string_of_int(i)

(* The result of an attempt to parse a snippet of code. *)
type parse_result =
    PR_error of src_loc_t * string
  | PR_success of expr_t

(* The result of an attempt to interpret an AST. *)
type interp_result =
    IR_error of src_loc_t * string
  | IR_success of value_t

(* Exception thrown by the lexer when an error is encountered. *)
exception LexicalExn of src_loc_t * string

(* Exception thrown by the interpreter when an error is encountered. *)
exception InterpExn of src_loc_t * string

