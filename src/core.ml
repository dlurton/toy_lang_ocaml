open Ast
open Lexer
open Lexing
open Errors

let add_scalars(left, right) =
  match (left, right) with
  | (Int32 lval, Int32 rval) -> Int32(lval + rval)
  | _ -> raise (InterpError("Incompatible data types"))

let eval e =
  let rec step = function
    | Var v               -> raise (InterpError ("Unbound variable '" ^ v ^ "'"))
    | Literal n           -> n
    | Add(l, r)             -> add_scalars(step(l), step(r))
    | _                   -> raise (InterpError ("Unsupported expression type"))
  in
  try InterpSuccess(step(e))
  with InterpError msg ->
    InterpError(msg)


(* Parse a string into an ast
   Note: error handling is described here: https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
*)

let parse s =
  let lexbuf = Lexing.from_string(s) in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    ParseSuccess(ast)
  with LexicalError msg ->
    ParseError("Lexical error: " ^ msg)
     | Parser.Error ->
       ParseError("Syntax error")

let scalar_to_int = function
  | Int32 i -> i
(*| _ -> failwith "Not an integer"*)

let scalar_to_string = function
  | Int32 i -> string_of_int(i)
(*| _ -> failwith "Not an integer"*)

(* TODO: fold in to pattern matched eval function *)
let execute (s: string) : int =
  let presult = parse(s) in
  match presult with
  | ParseError pmsg -> failwith("Parse error: " ^ pmsg)
  | ParseSuccess e ->
    let iresult = eval(e) in
    match iresult with
    | InterpError imsg -> failwith("Interp error: " ^ imsg)
    | InterpSuccess r -> scalar_to_int(r)


(* A few test cases *)
let run_tests ()  =
  assert (22 = execute "22");
  assert (22 = execute "11+11");
  assert (22 = execute "(10+1)+(5+6)");
  assert (22 = execute "let x = 22 in x");
  assert (23 = execute "let x = 0 in let x = 22 in x")

