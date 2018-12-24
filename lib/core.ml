open Types

(* An environment that has no variable and no parent. *)
let empty_env : env_t = fun _ -> None

(* Nests the specified env in another env that has a variable. *)
let extend_env env id value = fun search_id ->
  if id = search_id then
    Some(value)
  else
    env search_id

(*
   Nests the specified env in another env that has a variable that is backed by a lazily instantiated value.char_offset.
   This supports `let rec`.
*)
let rec extend_env_rec env id value_exp =
  let rec nested_env search_id =
     if id = search_id then
       Some(inner_eval value_exp nested_env)
     else
       env search_id
  in
  nested_env

(* Evaluates the parsed expression with the specified top-level environment. *)
 and inner_eval (e: expr_t) (env: env_t) =
    match e.exp with
    | EXPN_var id ->
      begin
        let value = env id in
        match value with
        | None -> raise (InterpExn(e.loc, ERR_unbound_var(id)))
        | Some v -> v
      end
    | EXPN_literal n -> n
    | EXPN_binary(op, left, right) ->
      begin
        let values = ((inner_eval left env), (inner_eval right env)) in
        let binary_int_op (func: int * int -> int) =
          begin
            match values with
            (* we have integers on both sides -- perform addition *)
            | (VAL_i32 lval, VAL_i32 rval) -> VAL_i32(func(lval, rval))
            (* non-integer on right side, use location of right hand expression *)
            | (VAL_i32 _, _) -> raise (InterpExn(right.loc, ERR_arithmetic_on_non_number))
            (* non-integer on left side, use location of left hand expression *)
            | (_, VAL_i32 _) -> raise (InterpExn(left.loc, ERR_arithmetic_on_non_number))
            (* non-integer on both sides, location of `e` *)
            | (_, _) -> raise (InterpExn(e.loc, ERR_arithmetic_on_non_number))
          end in
        match op with
        | OP_equals ->
          begin
            match values with
            | (VAL_i32 lval, VAL_i32 rval) -> VAL_bool(lval = rval)
            | (VAL_bool lval, VAL_bool rval) -> VAL_bool(lval = rval)
            | (_, _) -> VAL_bool(false)
          end
        | OP_add -> binary_int_op (fun (l, r) -> l + r)
        | OP_sub -> binary_int_op (fun (l, r) -> l - r)
        | OP_mul -> binary_int_op (fun (l, r) -> l * r)
        | OP_div -> binary_int_op (fun (l, r) -> l / r)
        | OP_mod -> binary_int_op (fun (l, r) -> l mod r)
      end
    | EXPN_if(cond_exp, then_exp, else_exp) ->
      let cond_val = inner_eval cond_exp env in
      begin
        match cond_val with
        | VAL_bool(true) -> inner_eval then_exp env
        | VAL_bool(false) -> inner_eval else_exp env
        | _ -> raise (InterpExn(cond_exp.loc, ERR_if_cond_not_bool))
      end
    | EXPN_let(id, value_exp, body_exp ) ->
      let the_value = inner_eval value_exp env in
      let nested_env = extend_env env id the_value in
      inner_eval body_exp nested_env
    | EXPN_let_rec(id, value_exp, body_exp) ->
      let nested_env = extend_env_rec env id value_exp in
      inner_eval body_exp nested_env
    | EXPN_func(id, body_exp) -> VAL_func(id, body_exp, env)
    | EXPN_call(func_exp, arg_exp) ->
      let proc_val = inner_eval func_exp env in
      begin
        match proc_val with
        | VAL_func(arg_name, body_exp, captured_env) ->
          let arg_value = inner_eval arg_exp env in
          let call_env = extend_env captured_env arg_name arg_value in
          inner_eval body_exp call_env
        | _ -> raise (InterpExn(e.loc, ERR_invoked_non_func))
      end

let eval e top_env : interp_result =
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


