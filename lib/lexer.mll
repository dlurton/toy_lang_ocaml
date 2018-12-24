
{
open Parser
open Types
open Util

(*
The ocamllex bootstrapped lexer seems like a good thing to use an
example for error handling: https://github.com/let-def/ocamllex/blob/master/lexer.mll
*)

let raise_lexical_error lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  let src_loc = src_loc_of_position p in
  raise (LexicalExn(src_loc, msg))
;;

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter+

rule read =
  parse
  | white   { read lexbuf }
  | "+"     { ADD }
  | "-"     { SUB }
  | "*"     { MUL }
  | "/"     { DIV }
  | "%"     { MOD }
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "let"   { LET }
  | "->"    { ARROW }
  | "func"  { FUNC }
  | "="     { EQUALS }
  | "in"    { IN }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | id      { ID (Lexing.lexeme lexbuf) }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof     { EOF }
  | _ {
      let illegal_c = Lexing.lexeme lexbuf in
      raise_lexical_error lexbuf ("Illegal character: " ^ illegal_c)
    }

(* And that's the end of the lexer definition. *)
