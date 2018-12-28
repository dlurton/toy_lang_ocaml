open Types
open Printf

let rec pretty_string_of_expr e =
  let sprintf = Printf.sprintf in
  match e.exp with
  | EXPN_var v -> sprintf "%s" v
  | EXPN_index(env_index, var_index) -> sprintf "|%d %d|" env_index var_index
  | EXPN_literal n ->
    let value_str = pretty_string_of_value n in
    sprintf "%s" value_str
  | EXPN_binary(op, l, r) ->
    let op = match op with
      | OP_add -> "+"
      | OP_sub -> "-"
      | OP_mul -> "*"
      | OP_div -> "/"
      | OP_mod -> "%"
      | OP_equals -> "="
    in
    let lvalue = pretty_string_of_expr l in
    let rvalue = pretty_string_of_expr r in
    sprintf " (%s) %s (%s) " lvalue op rvalue
  | EXPN_if(c, t, e) ->
    sprintf "if %s then %s else %s"
      (pretty_string_of_expr c)
      (pretty_string_of_expr t)
      (pretty_string_of_expr e)
  | EXPN_let(name, recursive, value_exp, body_exp) ->
    let value_str = pretty_string_of_expr value_exp in
    let body_str = pretty_string_of_expr body_exp in
    let rec_str = if recursive then "rec " else "" in
    sprintf "let %s%s = %s in %s" rec_str name value_str body_str
  | EXPN_call(func_exp, arg_exps) ->
    sprintf "%s(%s)"
      (pretty_string_of_expr func_exp)
      (String.concat " " (List.map (fun e -> pretty_string_of_expr e) arg_exps))
  | EXPN_func(arg_names, body_exp) ->
    sprintf "func %s -> %s"
      (String.concat " " arg_names)
      (pretty_string_of_expr body_exp)
and pretty_string_of_value = function
  | VAL_bool b -> string_of_bool b
  | VAL_i32 i -> string_of_int i
  | VAL_func(arg_count, body_exp, _) ->
    sprintf "func_value (%d args) -> %s"
      arg_count
      (pretty_string_of_expr body_exp)
  | VAL_ref r ->
    sprintf "(ref %s)" (pretty_string_of_value !r)
