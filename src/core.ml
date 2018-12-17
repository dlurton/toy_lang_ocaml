open Types
open Lexer
open Lexing
open Parsing

(*
   Menhir reference manual: http://gallium.inria.fr/~fpottier/menhir/manual.pdf
   Lexing module documentation: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html
*)


(* An environment that has no variable and no parent. *)
let empty_env = fun _ -> None

(* Nests the specified env in another env that has a variable. *)
let extend_env env name value = fun lookup ->
  match lookup with
  | name -> Some(value)

(* Evaluates the parsed expression with the specified top-level environment. *)
let eval e top_env : interp_result =
  let add_scalars left right  =
    match (left, right) with
    | (Int32Value lval, Int32Value rval) -> Int32Value(lval + rval)
    | (ProcValue _, _) -> failwith("TODO")
    | ( _, ProcValue _) -> failwith("TODO")
  in
  let rec inner_eval (e: expr) (env: string -> value option) : value =
    match e.exp with
    | Var v ->
      begin
        let value = env(v) in
        match value with
        (* TODO: don't throw an exception here? *)
        | None -> raise (InterpExn(e.loc, "Unbound variable '" ^ v ^ "'"))
        | Some v -> v
      end
    | Literal n -> n
    | Proc(name, body_exp) -> ProcValue(name, body_exp)
    | Add(l, r) ->
      let lvalue = inner_eval l env in
      let rvalue = inner_eval r env in
      add_scalars lvalue rvalue
    | Let(name, value_exp, body_exp) ->
      let the_value = inner_eval value_exp env in
      let nested_env = extend_env env name the_value in
      inner_eval body_exp nested_env
    | Call(proc_exp, arg_exp) ->
      let proc_val = inner_eval proc_exp env in
      match proc_val with
      | ProcValue(arg_name, body_exp) ->
        let arg_value = inner_eval arg_exp env in
        let call_env = extend_env env arg_name arg_value in
        inner_eval body_exp call_env
      | _ -> failwith "TODO: error handling when proc expr is not a proc"
  in
  try InterpSuccess(inner_eval e top_env)
  with InterpExn (loc, msg) ->
    InterpError(loc, msg)

(* Evaluates the parsed expression with an empty environment. *)
let eval_with_empty_env e =
  eval e empty_env

(*
   Uses the Menhir generated parser to turn a string into an AST.
   Note: error handling is described here: https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
*)
let parse s =
  let lexbuf = Lexing.from_string(s) in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    ParseSuccess(ast)
  with LexicalExn(src_loc, msg) ->
    ParseError(src_loc, "Lexical error: " ^ msg)
    | Parser.Error ->
       ParseError(make_source_location "TODO" 1 1, "Syntax error")

