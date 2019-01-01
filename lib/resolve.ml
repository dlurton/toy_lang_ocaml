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


let resolve_rewrite (expr: expr_node_t) =
  let rec inner_resolve_rewrite e senv =
    let new_e = match e.exp with
      (* let evaluates value_exp, then extends the environment
         with the result, and executes body_exp under this
         extended environment *)
      | EXPN_let(vd, body_exp) ->
        let (id, ty, value_exp) = vd in
        let let_senv = extend_senv [id] senv in
        Some(EXPN_let(
          (id, ty, (rewrite value_exp senv inner_resolve_rewrite)),
          (rewrite body_exp let_senv inner_resolve_rewrite)
        ))
      (* let rec is similar to let, but executes both value_exp
         and body_exp in the extended environment so that
         value_exp has access to the variable being defined.
         let rec also supports multiple variable definitions. *)
      | EXPN_let_rec(var_defs, body_exp) ->
        let ids = var_defs |> List.map (fun vd -> let (id, _, _) = vd in id) in
        let let_senv = extend_senv ids senv in
        let new_var_defs =
          var_defs |> List.map
            (fun vd ->
               let (id, ty, value_exp) = vd in
               (id, ty, (rewrite value_exp let_senv inner_resolve_rewrite)))
        in
        Some(EXPN_let_rec(
            new_var_defs,
            (rewrite body_exp let_senv inner_resolve_rewrite)
          ))
      | EXPN_func(arg_defs, ret_type, body_exp) ->
        let ids = arg_defs |> List.map (fun vd -> let (id, _) = vd in id) in
        let arg_senv = extend_senv ids senv in
        Some(EXPN_func(
            arg_defs,
            ret_type,
            (rewrite body_exp arg_senv inner_resolve_rewrite)))
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
