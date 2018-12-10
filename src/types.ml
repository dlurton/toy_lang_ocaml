


type scalar_value =
  | Int32 of int


type expr =
  | Var of string
  | Literal of scalar_value
  | Add of expr * expr
  | Let of string * expr * expr


type parse_result =
    ParseError of string
  | ParseSuccess of expr


type interp_result =
    InterpError of string
  | InterpSuccess of scalar_value


exception LexicalExn of string
exception InterpExn of string


