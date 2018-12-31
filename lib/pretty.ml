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
  | EXPN_logical(lop, l, r) ->
    sprintf "(%s) %s (%s)"
      (pretty_string_of_expr l)
      (match lop with
      | LOP_and   -> "&&"
      | LOP_or    -> "||")
      (pretty_string_of_expr r)
  | EXPN_binary(op, l, r) ->
    sprintf " (%s) %s (%s) "
      (pretty_string_of_expr l)
      (match op with
      | OP_add    -> "+"
      | OP_sub    -> "-"
      | OP_mul    -> "*"
      | OP_div    -> "/"
      | OP_mod    -> "%"
      | OP_eq     -> "="
      | OP_gt     -> ">"
      | OP_gte    -> ">="
      | OP_lt     -> "<"
      | OP_lte    -> "<=")
      (pretty_string_of_expr r)
  | EXPN_if(c, t, e) ->
    sprintf "if %s then %s else %s"
      (pretty_string_of_expr c)
      (pretty_string_of_expr t)
      (pretty_string_of_expr e)
  | EXPN_let(var_def, body_exp) ->
    sprintf "let %s in %s"
      (pretty_string_of_var_def var_def)
      (pretty_string_of_expr body_exp)
  | EXPN_let_rec(var_defs, body_exp) ->
    let var_def_strs =
      var_defs |> List.map (fun vd -> pretty_string_of_var_def vd)
    in
    sprintf "let rec %s in %s"
      (String.concat " and " var_def_strs)
      (pretty_string_of_expr body_exp)
  | EXPN_call(func_exp, arg_exps) ->
    sprintf "%s(%s)"
      (pretty_string_of_expr func_exp)
      (String.concat " " (List.map (fun e -> pretty_string_of_expr e) arg_exps))
  | EXPN_func(arg_defs, ret_type, body_exp) ->
    sprintf "func(%s) -> %s %s"
      (String.concat ", "
         (arg_defs |> List.map
            (fun ad -> let (id, ty) = ad in
              sprintf "%s: %s" id (pretty_string_of_type ty))))
      (pretty_string_of_type ret_type)
      (pretty_string_of_expr body_exp)
and pretty_string_of_value = function
  | VAL_bool b -> string_of_bool b
  | VAL_int i -> string_of_int i
  | VAL_func(arg_count, body_exp, _) ->
    sprintf "func_value (%d args) -> %s"
      arg_count
      (pretty_string_of_expr body_exp)
  | VAL_type t -> pretty_string_of_type t
and pretty_string_of_type = function
  | TY_int -> "int"
  | TY_bool -> "bool"
  | TY_func (arg_types, ret_type) ->
    sprintf "(%s): %s"
      (String.concat ", " (arg_types |> List.map (fun t -> pretty_string_of_type t)))
      (pretty_string_of_type ret_type)
and pretty_string_of_var_def vd =
  let (name, ty, value_exp) = vd in
  sprintf "%s: %s = %s"
    name
    (pretty_string_of_type ty)
    (pretty_string_of_expr value_exp)



