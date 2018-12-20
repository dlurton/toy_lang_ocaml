open Types

(* An environment that has no variable and no parent. *)
let empty_env = fun _ -> None

(* Nests the specified env in another env that has a variable. *)
let extend_env env id value = fun lookup ->
  if String.equal id lookup then Some(value) else env lookup

(* Evaluates the parsed expression with the specified top-level environment. *)
let eval e top_env : interp_result =
  let add_scalars left right  =
    match (left, right) with
    | (Int32 lval, Int32 rval) -> Int32(lval + rval)
  in
  let rec innerEval (e: expr_t) (env: string -> value_t option) : value_t =
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
    | Add(l, r) ->
      let lvalue = innerEval l env in
      let rvalue = innerEval r env in
      add_scalars lvalue rvalue
    | Let({ id; value_exp; body_exp }) ->
      let the_value = innerEval value_exp env in
      let nested_env = extend_env env id the_value in
      innerEval body_exp nested_env
  in
  try InterpSuccess(innerEval e top_env)
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
       ParseError(make_src_loc "TODO" 1 1, "Syntax error")



