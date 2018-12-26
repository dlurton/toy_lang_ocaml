open Types

type lexaddr_t
type senv_t = string list

(* An empty static environment. *)
let empty_senv : senv_t = []

(* Extend a static environment. *)
let extend_senv id senv : senv_t =
  id :: senv

let senv_lookup id senv =
  let rec search_senv index next_senv =
    if next_senv = [] then None
    else if id = (List.hd next_senv) then Some(index)
    else search_senv (index + 1)(List.tl next_senv)
  in
    search_senv 0 senv

let resolve_rewrite (expr: expr_t) : expr_t =
  let rec resolve_rewrite_expr (e: expr_t) (senv: senv_t) =
  (*
     TODO: this function contains a lot of boilerplate.
     Only the EXPN_index, EXPN_var and EXPN_let nodes are unique
     to this particular rewriter.  How do we factor out this
     boiler plate so that it can be reused by other rewriters?
  *)
    {
      loc = e.loc;
      exp = match e.exp with
        | EXPN_index _ -> failwith "This AST already has at least one index."
        | EXPN_var id ->
          begin
            match senv_lookup id senv with
            | None -> raise (InterpExn(e.loc, ERR_unbound_var(id)))
            | Some index -> EXPN_index(index)
          end
        | EXPN_literal n -> EXPN_literal(n)
        | EXPN_binary(op, left, right) -> EXPN_binary(
            op,
            (resolve_rewrite_expr left senv),
            (resolve_rewrite_expr right senv)
          )
        | EXPN_if(cond_exp, then_exp, else_exp) -> EXPN_if(
            (resolve_rewrite_expr cond_exp senv),
            (resolve_rewrite_expr then_exp senv),
            (resolve_rewrite_expr else_exp senv)
          )
        | EXPN_let(id, recursive, value_exp, body_exp ) ->
          (* the new static environment defined by the let *)
          let let_senv = extend_senv id senv in
          (* the static environment of the value expression *)
          let senv_for_value = if not recursive then
              senv
            else
              let_senv
          in
          (* the static environment of the body *)
          let senv_for_body = extend_senv id senv_for_value in
          EXPN_let(
            id,
            recursive,
            (resolve_rewrite_expr value_exp senv_for_value),
            (resolve_rewrite_expr body_exp senv_for_body)
          )
        | EXPN_func(arg_id, body_exp) -> EXPN_func(
            arg_id,
            extend_senv arg_id senv |> resolve_rewrite_expr body_exp
          )
        | EXPN_call(func_exp, arg_exp) -> EXPN_call(
            (resolve_rewrite_expr func_exp senv),
            (resolve_rewrite_expr arg_exp senv)
          )
    }
  in resolve_rewrite_expr expr empty_senv

