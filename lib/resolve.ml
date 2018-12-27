open Types
open Rewrite

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

let resolve_rewrite (expr: expr_t) =
  let rec inner_resolve_rewrite e senv =
    let new_e = match e.exp with
      | EXPN_let(id, recursive, value_exp, body_exp ) ->
        (* the new static environment defined by the let *)
        let let_senv = extend_senv id senv in
        (* the static environment of the value expression *)
        let senv_for_value = if not recursive then senv else let_senv in
        (* the static environment of the body *)
        let senv_for_body = extend_senv id senv_for_value in
        Some(EXPN_let(
          id,
          recursive,
          (rewrite value_exp senv_for_value inner_resolve_rewrite),
          (rewrite body_exp senv_for_body inner_resolve_rewrite)
        ))
      | EXPN_func(arg_id, body_exp) ->
        let arg_senv = extend_senv arg_id senv in
        Some(EXPN_func(
          arg_id,
          (rewrite body_exp arg_senv inner_resolve_rewrite)
        ))
      | EXPN_index _ -> failwith "This AST already has at least one index."
      | EXPN_var id ->
        begin
          match senv_lookup id senv with
          | None -> raise (InterpExn(e.loc, ERR_unbound_var(id)))
          | Some index -> Some(EXPN_index(index))
        end
      | _ -> None
    in
    match new_e with
    | None -> None
    | Some new_node -> Some({ expr with exp = new_node })
  in
  rewrite expr empty_senv inner_resolve_rewrite
