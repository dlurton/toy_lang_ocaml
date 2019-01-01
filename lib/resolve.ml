open Types
open Rewrite

type senv_entry_t = string * type_t

type senv_t = senv_entry_t list list

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
      | hd::tl -> let (candidate_id, candidate_ty) = hd in
        if candidate_id = id then Some(var_index, candidate_ty) else search_vars tl (var_index + 1)
    in
    match senv with
    | [] -> None  (* searched all the way to global scope, variable not found *)
    | hd::tl ->
      begin (* search the next senv *)
        match search_vars hd 0 with
        | Some(var_index, var_ty) -> Some(env_index, var_index, var_ty)
        | None -> search_senv tl (env_index + 1)
      end
  in
  search_senv top_senv 0


let resolve_rewrite (node: expr_node_t) =
  let rec inner_resolve_rewrite e senv =
    let new_e = match e.exp with
      (* let evaluates value_exp, then extends the environment
         with the result, and executes body_exp under this
         extended environment *)
      | EXP_let(vd, body_exp) ->
        let (id, ty, value_exp) = vd in
        let let_senv = extend_senv [(id, ty)] senv in
        Some(EXP_let(
          (id, ty, (rewrite value_exp senv inner_resolve_rewrite)),
          (rewrite body_exp let_senv inner_resolve_rewrite)
        ))
      (* let rec is similar to let, but executes both value_exp
         and body_exp in the extended environment so that
         value_exp has access to the variable being defined.
         let rec also supports multiple variable definitions. *)
      | EXP_let_rec(var_defs, body_exp) ->
        let ids = var_defs |> List.map (fun vd -> let (id, ty, _) = vd in (id, ty)) in
        let let_senv = extend_senv ids senv in
        let new_var_defs =
          var_defs |> List.map
            (fun vd ->
               let (id, ty, value_exp) = vd in
               (id, ty, (rewrite value_exp let_senv inner_resolve_rewrite)))
        in
        Some(EXP_let_rec(
            new_var_defs,
            (rewrite body_exp let_senv inner_resolve_rewrite)
          ))
      | EXP_func(arg_defs, ret_type, body_exp) ->
        let ids = arg_defs |> List.map (fun vd -> let (id, ty) = vd in (id, ty)) in
        let arg_senv = extend_senv ids senv in
        Some(EXP_func(
            arg_defs,
            ret_type,
            (rewrite body_exp arg_senv inner_resolve_rewrite)))
      | EXP_index _ -> failwith "This AST already has at least one index."
      | EXP_var id ->
        begin
          match senv_lookup id senv with
          | None -> raise (InterpExn(e.loc, ERR_unbound_var(id)))
          | Some (e_index, v_index, v_ty) -> Some(EXP_index(e_index, v_index, v_ty))
        end
      | _ -> None
    in
    match new_e with
    | None -> None
    | Some new_node -> Some({ node with exp = new_node })
  in
  rewrite node empty_senv inner_resolve_rewrite
