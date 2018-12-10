open Types
open Lexer
open Lexing

let add_scalars left right  =
  match (left, right) with
  | (Int32 lval, Int32 rval) -> Int32(lval + rval)
  | _ -> raise (InterpExn("Incompatible data types"))

let empty_env = fun _ -> None

let extend_env env name value = fun lookup ->
  match lookup with
  | name -> Some(value)
  | _ -> env lookup

let eval e top_env : interp_result =
  let rec innerEval (e: expr) (env: string -> scalar_value option) : scalar_value =
    match e with
    | Var v ->
      begin
        let value = env(v) in
        match value with
        (* TODO: don't throw an exception here? *)
        | None -> raise (InterpExn("Unbound variable '" ^ v ^ "'"))
        | Some v -> v
      end
    | Literal n -> n
    | Add(l, r) ->
      let lvalue = innerEval l env in
      let rvalue = innerEval r env in
      add_scalars lvalue rvalue
    | Let(name, valueExp, bodyExp) ->
      let the_value = innerEval valueExp env in
      let nested_env = extend_env env name the_value in
      innerEval bodyExp nested_env
  in
  try InterpSuccess(innerEval e top_env)
  with InterpExn msg ->
    InterpError(msg)


(* Parse a string into an ast
   Note: error handling is described here: https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
*)

let parse s =
  let lexbuf = Lexing.from_string(s) in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    ParseSuccess(ast)
  with LexicalExn msg ->
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
    let iresult = eval e empty_env in
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

