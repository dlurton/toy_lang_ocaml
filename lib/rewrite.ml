open Types

type ('cx) custom_rewriter_t = expr_t -> 'cx -> expr_t option

let rewrite outer_e (outer_ctx: 'cx) (custom_rewrite: 'cx custom_rewriter_t) =
  let rec inner_rewrite e ctx =
    match custom_rewrite e ctx with
        | Some (new_e) -> new_e
        | None -> {
            loc = e.loc;
            exp =
              match e.exp with
              | EXPN_var id -> EXPN_var(id)
              | EXPN_index (i, j) -> EXPN_index(i, j)
              | EXPN_literal n -> EXPN_literal(n)
              | EXPN_logical(lop, left, right) -> EXPN_logical(
                  lop,
                  (inner_rewrite left ctx),
                  (inner_rewrite right ctx)
                )
              | EXPN_binary(op, left, right) -> EXPN_binary(
                  op,
                  (inner_rewrite left ctx),
                  (inner_rewrite right ctx)
                )
              | EXPN_if(cond_exp, then_exp, else_exp) -> EXPN_if(
                  (inner_rewrite cond_exp ctx),
                  (inner_rewrite then_exp ctx),
                  (inner_rewrite else_exp ctx)
                )
              | EXPN_let(id, recursive, value_exp, body_exp ) ->
                EXPN_let(
                  id,
                  recursive,
                  (inner_rewrite value_exp ctx),
                  (inner_rewrite body_exp ctx)
                )
              | EXPN_func(arg_id, body_exp) -> EXPN_func(
                  arg_id,
                  (inner_rewrite body_exp ctx)
                )
              | EXPN_call(func_exp, arg_exps) -> EXPN_call(
                  (inner_rewrite func_exp ctx),
                  arg_exps |> List.map (fun e -> inner_rewrite e ctx)
                )
          }
  in
  inner_rewrite outer_e outer_ctx

let default_rewrite e =
  rewrite e () (fun _ _ -> None)

