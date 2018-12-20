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
    | (VAL_i32 lval, VAL_i32 rval) -> VAL_i32(lval + rval)
    | _ -> failwith ("One or more values cannot be added")
  in
  let rec inner_eval (e: expr_t) (env: string -> value_t option) : value_t =
    match e.exp with
    | EXPN_var v ->
      begin
        let value = env(v) in
        match value with
        (* TODO: don't throw an exception here? *)
        | None -> raise (InterpExn(e.loc, "Unbound variable '" ^ v ^ "'"))
        | Some v -> v
      end
    | EXPN_literal n -> n
    | EXPN_add(l, r) ->
      let lvalue = inner_eval l env in
      let rvalue = inner_eval r env in
      add_scalars lvalue rvalue
    | EXPN_let(id, value_exp, body_exp ) ->
      let the_value = inner_eval value_exp env in
      let nested_env = extend_env env id the_value in
      inner_eval body_exp nested_env
    | EXPN_func(id, body_exp) -> VAL_func(id, body_exp)
    | EXPN_call(func_exp, arg_exp) ->
      let proc_val = inner_eval func_exp env in
      match proc_val with
      | VAL_func(arg_name, body_exp) ->
        let arg_value = inner_eval arg_exp env in
        let call_env = extend_env env arg_name arg_value in
        inner_eval body_exp call_env
      | _ -> failwith "TODO: error handling when proc expr is not a proc"

  in
  try IR_success(inner_eval e top_env)
  with InterpExn (loc, msg) ->
    IR_error(loc, msg)

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
    PR_success(ast)
  with LexicalExn(src_loc, msg) ->
    PR_error(src_loc, "Lexical error: " ^ msg)
    | Parser.Error ->
       PR_error(make_src_loc "TODO" 1 1, "Syntax error")



