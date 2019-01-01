open Types

type ('cx) custom_rewriter_t = expr_node_t -> 'cx -> expr_node_t option

let rewrite outer_e (outer_ctx: 'cx) (custom_rewrite: 'cx custom_rewriter_t) =
  let rec inner_rewrite e ctx =
    match custom_rewrite e ctx with
        | Some (new_e) -> new_e
        | None -> {
            loc = e.loc;
            exp =
              match e.exp with
              | EXP_var id -> EXP_var(id)
              | EXP_index (env_index, var_index, var_type) ->
                EXP_index(env_index, var_index, var_type)
              | EXP_literal n -> EXP_literal(n)
              | EXP_logical(lop, left, right) -> EXP_logical(
                  lop,
                  (inner_rewrite left ctx),
                  (inner_rewrite right ctx)
                )
              | EXP_binary(op, left, right) -> EXP_binary(
                  op,
                  (inner_rewrite left ctx),
                  (inner_rewrite right ctx)
                )
              | EXP_if(cond_exp, then_exp, else_exp) -> EXP_if(
                  (inner_rewrite cond_exp ctx),
                  (inner_rewrite then_exp ctx),
                  (inner_rewrite else_exp ctx)
                )
              | EXP_let(vd, body_exp) ->
                let (id, ty, value_exp) = vd in
                EXP_let(
                  (id, ty, (inner_rewrite value_exp ctx)),
                  (inner_rewrite body_exp ctx)
                )
              | EXP_let_rec(var_defs, body_exp ) ->
                let new_var_defs =
                  var_defs |> List.map
                    (fun vd -> let (id, ty, value_exp) = vd in
                      (id, ty, (inner_rewrite value_exp) ctx))
                in
                EXP_let_rec(
                  new_var_defs,
                  (inner_rewrite body_exp ctx)
                )
              | EXP_func(arg_defs, ret_type, body_exp) -> EXP_func(
                  arg_defs,
                  ret_type,
                  (inner_rewrite body_exp ctx)
                )
              | EXP_call(func_exp, arg_exps) -> EXP_call(
                  (inner_rewrite func_exp ctx),
                  arg_exps |> List.map (fun e -> inner_rewrite e ctx)
                )
          }
  in
  inner_rewrite outer_e outer_ctx

let default_rewrite e =
  rewrite e () (fun _ _ -> None)

