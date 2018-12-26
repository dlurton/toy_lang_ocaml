open Types
open Printf

let rec pretty_string_of_expr e =
  let sprintf = Printf.sprintf in
  match e.exp with
  | EXPN_var v -> sprintf "%s" v
  | EXPN_index i -> sprintf "|%d|" i
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
  | EXPN_call(proc_exp, arg_exp) ->
    let proc_str = pretty_string_of_expr proc_exp in
    let body_str = pretty_string_of_expr arg_exp in
    sprintf "%s(%s)" proc_str body_str
  | EXPN_func(arg_name, body_exp) ->
    let body_str = pretty_string_of_expr body_exp in
    sprintf "func %s -> %s" arg_name body_str
and pretty_string_of_value = function
  | VAL_bool b -> string_of_bool b
  | VAL_i32 i -> string_of_int i
  | VAL_func(arg_name, body_exp, _) ->
    let body_str = pretty_string_of_expr body_exp in
    sprintf "func_value %s -> %s" arg_name body_str
  | VAL_delayed_val _ -> "(*delayed value*)"
