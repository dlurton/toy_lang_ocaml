
open Types

open Printf

let rec pretty_string_of_expr e =
  let sprintf = Printf.sprintf in
    match e.exp with
      | Var v ->
        sprintf "%s" v
      | Literal n ->
        let value_str = pretty_string_of_value n in
        sprintf "%s" value_str
    | Add(l, r) ->
      let lvalue = pretty_string_of_expr l in
      let rvalue = pretty_string_of_expr r in
      sprintf " %s + %s " lvalue rvalue
    | Let(name, value_exp, body_exp) ->
      let value_str = pretty_string_of_expr value_exp in
      let body_str = pretty_string_of_expr body_exp in
      sprintf "\nlet %s = %s in %s" name value_str body_str
    | Call(proc_exp, arg_exp) ->
      let proc_str = pretty_string_of_expr proc_exp in
      let body_str = pretty_string_of_expr arg_exp in
      sprintf "%s(%s)" proc_str body_str
    | Proc(arg_name, body_exp) ->
      let body_str = pretty_string_of_expr body_exp in
      sprintf "proc(%s) %s" arg_name body_str
and pretty_string_of_value = function
   | Int32Value i -> string_of_int i
   | ProcValue(arg_name, body_exp) ->
      let body_str = pretty_string_of_expr body_exp in
           sprintf "proc(%s) %s" arg_name body_str


