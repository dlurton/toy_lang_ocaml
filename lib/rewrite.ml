open Types


type rewriter_t = expr_t -> expr_t

let rewrite_expr (e: expr_t) (rewrite_expr: rewriter_t) =
  {
    loc = e.loc;
    exp = match e.exp with
      | EXPN_var id -> EXPN_var(id)
      | EXPN_literal n -> EXPN_literal(n)
      | EXPN_binary(op, left, right) -> EXPN_binary(
          op,
          (rewrite_expr left),
          (rewrite_expr right)
        )
      | EXPN_if(cond_exp, then_exp, else_exp) -> EXPN_if(
          (rewrite_expr cond_exp),
          (rewrite_expr then_exp),
          (rewrite_expr else_exp)
        )
      | EXPN_let(id, value_exp, body_exp ) -> EXPN_let(
          id,
          (rewrite_expr value_exp),
          (rewrite_expr body_exp)
        )
      | EXPN_let_rec(id, value_exp, body_exp) -> EXPN_let_rec(
          id,
          (rewrite_expr value_exp),
          (rewrite_expr body_exp)
        )
      | EXPN_func(id, body_exp) -> EXPN_func(
          id,
          (rewrite_expr body_exp)
        )
      | EXPN_call(func_exp, arg_exp) -> EXPN_call(
          (rewrite_expr func_exp),
          (rewrite_expr arg_exp)
        )
  }

let default_rewrite_expr e =
  let rec apply_rewrite e2 = rewrite_expr e2 apply_rewrite in
  apply_rewrite e



