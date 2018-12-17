(*
   This file contain the core data type definitions.
*)

type source_location = {
  file: string;
  line_num: int;
  char_offset: int;
}

let make_source_location file line_num char_offset = {
  file = file;
  line_num = line_num;
  char_offset = char_offset
}

let dummy_source_location = {
  file = "TODO";
  line_num = -1;
  char_offset = -1;
}

(* The evaluation-time "ground" types. *)
type value =
  | Int32Value of int
  | ProcValue of string * expr
  (* The AST. *)
and expr = {
    exp : expr_node;
    loc : source_location;
  }
and expr_node =
  | Var of string
  | Literal of value
  | Add of expr * expr
  | Let of string * expr * expr
  | Proc of string * expr
  | Call of expr * expr

let value_to_int = function
  | Int32Value i -> i
  | ProcValue _ -> failwith "TODO"

(* The result of an attempt to parse a snippet of code. *)
type parse_result =
    ParseError of source_location * string
  | ParseSuccess of expr


(* The result of an attempt to interpret an AST. *)
type interp_result =
    InterpError of source_location * string
  | InterpSuccess of value


(* Exception thrown by the lexer when an error is encountered. *)
exception LexicalExn of source_location * string

(* Exception thrown by the interpreter when an error is encountered. *)
exception InterpExn of source_location * string

