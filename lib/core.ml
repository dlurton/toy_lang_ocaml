open Types

(*TODO: update docs *)
let empty_env : env_t = []
let extend_env env value : env_t = value :: env

let apply_env env env_index var_index = (List.nth env env_index).(var_index)

let dump_env env =
  let rec inner_dump_env ienv idepth =
    match ienv with
    | [] -> ()
    | _ -> Printf.printf "%d: %s\n" idepth (List.hd ienv |> Pretty.pretty_string_of_value);
    inner_dump_env (List.tl ienv) (idepth + 1)
  in
  print_string "Environment Dump:\n";
  inner_dump_env env 0;
  flush stdout

(* Evaluates the parsed expression with the specified top-level environment. *)
let rec inner_eval (e: expr_t) (env: env_t) : value_t =
  match e.exp with
  | EXPN_var id -> failwith ("Variable '" ^ id ^ "' still existed for some reason")
  | EXPN_index (env_index, var_index) ->
    let value =
      apply_env env env_index var_index in
    begin
      match value with
      | VAL_ref r -> !r (* For now, automatically dereference. *)
      | x -> x
    end
  | EXPN_literal n -> n
  | EXPN_binary(op, left, right) ->
    let values = ((inner_eval left env), (inner_eval right env)) in
    let binary_int_op (func: int * int -> int) =
      begin
        match values with
        (* we have integers on both sides -- perform addition *)
        | (VAL_i32 lval, VAL_i32 rval) -> VAL_i32(func(lval, rval))
        | _ ->
            (* we have a non-integer somewhere *)
            raise (InterpExn(e.loc, ERR_arithmetic_on_non_number))
      end in
    begin
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
    | EXPN_let(_, recursive, value_exp, body_exp ) ->
      if not recursive then
        let the_value = inner_eval value_exp env in
        let nested_env = extend_env env [|the_value|] in
        inner_eval body_exp nested_env
      else
        let future_val = ref (VAL_i32(0)) in (* provide a dummy value *)
        let nested_env = extend_env env [|(VAL_ref(future_val))|] in
        future_val := inner_eval value_exp nested_env;
        inner_eval body_exp nested_env
    | EXPN_func(ids, body_exp) -> VAL_func(ids, body_exp, env)
    | EXPN_call(func_exp, arg_exps) ->
      let proc_val = inner_eval func_exp env in
      begin
        match proc_val with
        | VAL_func(_, body_exp, captured_env) ->
          let arg_values = Array.of_list
              (List.map (fun e -> inner_eval e env) arg_exps)
          in
          let call_env = extend_env captured_env arg_values in
          inner_eval body_exp call_env
        | _ -> raise (InterpExn(e.loc, ERR_invoked_non_func))
      end

let eval e top_env : interp_result =
  try
    let resolved_exp = e |> Resolve.resolve_rewrite in
    (*Printf.printf "\n****\n%s\n***" (Pretty.pretty_string_of_expr resolved_exp);*)
      IR_success(inner_eval resolved_exp top_env)
  with InterpExn (loc, msg) ->
    IR_error(loc, msg)

(* Evaluates the parsed expression with an empty environment. *)
let eval_with_empty_env e =
  eval e empty_env

(* Uses the Menhir generated parser to turn a string into an AST.
   Note: error handling is described here:
   https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
let parse s =
  let lexbuf = Lexing.from_string(s) in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    PR_success(ast)
  with LexicalExn(src_loc, msg) ->
    PR_error(src_loc, "Lexical error: " ^ msg)
    | Parser.Error ->
       PR_error(make_src_loc "TODO" 1 1, "Syntax error")


