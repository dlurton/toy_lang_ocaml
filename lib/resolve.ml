open Types
open Rewrite

type senv_t = string list list

(* An empty static environment. *)
let empty_senv : senv_t = []

(* Extend a static environment. *)
let extend_senv ids senv : senv_t =
  ids :: senv

let senv_lookup id top_senv =
  let rec search_senv senv env_index =
    let rec search_vars vars var_index =
      match vars with
      | [] -> None (* variable not found in vars *)
      | hd::tl -> if hd = id then Some(var_index) else search_vars tl (var_index + 1)
    in
    match senv with
    | [] -> None  (* searched all the way to global scope, variable not found *)
    | hd::tl ->
      begin (* search the next senv *)
        match search_vars hd 0 with
        | Some(var_index) -> Some(env_index, var_index)
        | None -> search_senv tl (env_index + 1)
      end
  in
  search_senv top_senv 0


let resolve_rewrite (expr: expr_t) =
  let rec inner_resolve_rewrite e senv =
    let new_e = match e.exp with
      | EXPN_let(id, recursive, value_exp, body_exp ) ->
        (* the new static environment defined by the let *)
        let let_senv = extend_senv [id] senv in
        (* the static environment of the value expression *)
        let senv_for_value = if not recursive then senv else let_senv in
        Some(EXPN_let(
          id,
          recursive,
          (rewrite value_exp senv_for_value inner_resolve_rewrite),
          (rewrite body_exp let_senv inner_resolve_rewrite)
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
          | Some (e_index, v_index) -> Some(EXPN_index(e_index, v_index))
        end
      | _ -> None
    in
    match new_e with
    | None -> None
    | Some new_node -> Some({ expr with exp = new_node })
  in
  rewrite expr empty_senv inner_resolve_rewrite
