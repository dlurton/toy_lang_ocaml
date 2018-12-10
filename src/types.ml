(*
   This file contain the core data type definitions.
*)

(* The evaluation-time "ground" types. *)
type value =
  | Int32 of int

let value_to_int = function
  | Int32 i -> i

let value_to_string = function
  | Int32 i -> string_of_int(i)

(* The AST. *)
type expr =
  | Var of string
  | Literal of value
  | Add of expr * expr
  | Let of string * expr * expr


(* The result of an attempt to parse a snippet of code. *)
type parse_result =
    ParseError of string
  | ParseSuccess of expr


(* The result of an attempt to interpret an AST. *)
type interp_result =
    InterpError of string
  | InterpSuccess of value


(* Exception thrown by the lexer when an error is encountered. *)
exception LexicalExn of string

(* Exception thrown by the interpreter when an error is encountered. *)
exception InterpExn of string

