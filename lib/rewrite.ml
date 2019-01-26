open Types

(**
   Most AST transformations are only focused on a a subset of expr_node_t
   variants.Those notes which are not specially modified during an AST
   transform are usually a deep clone.  The way I favor for accmoplishing a
   deep clone is to pattern match over the expr_node_t variant.  However, it
   would be a huge waste of bytes to copy & paste a huge match expression and
   modify it minimally for the purposes of a specific rewrite.

   This class is an attempt to mitigate that.  It provides a default rewriter
   which deeply clones the expr_node_t which is passed to the rewrite method,
   however before each node is cloned custom_rewrite is called to see if
   the derived class can apply a specific rewrite to the node.  If
   custom_rewrite returns null then the default deep clone is performed
   instead.

   The derived class should call back into ctx_rewrite to rewrite its children
   nodes or apply the default deep clone as needed.
*)
class virtual ['ctx] expr_node_transform = object(self)
  method private virtual default_ctx: 'ctx

  (* TODO consider renaming custom_rewrite and custom_rewrite_type for more...
     symmetry *)

  (** Overriding methods may return Some to rewrite the specified node, or
      None to indicate that the default deep clone should be applied. *)
  method private custom_rewrite
      (_: expr_node_t)
      (_: 'ctx)
    : expr_node_t option = None

  method private rewrite_type (ty: type_t) =
    match ty with
    | TY_var(index) -> TY_var(index)
    | TY_bool -> TY_bool
    | TY_int -> TY_int
    | TY_func(func_ty) ->
      let (arg_tys, ret_ty) = func_ty in
      let new_arg_tys =
        arg_tys |> (List.map (fun t -> (self#rewrite_type t))) in
      let new_ret_ty = self#rewrite_type ret_ty in
      TY_func(new_arg_tys, new_ret_ty)

  (**
     Applies the custom rewrite or deeply clones the specified expr_node_t.
  *)
  method ctx_rewrite (e: expr_node_t) (ctx: 'ctx) =
    (* try the custom rewrite *)
    match self#custom_rewrite e ctx with
    | Some (new_e) -> new_e
    | None -> {
        loc = e.loc;
        exp =
          (* apply the default clone *)
          match e.exp with
          | EXP_var id -> EXP_var(id)
          | EXP_index (env_index, var_index, var_type) ->
            EXP_index(
              env_index,
              var_index,
              (self#rewrite_type var_type))
          | EXP_literal n -> EXP_literal(n)
          | EXP_logical(lop, left, right) ->
            EXP_logical(
              lop,
              (self#ctx_rewrite left ctx),
              (self#ctx_rewrite right ctx)
            )
          | EXP_binary(op, left, right) ->
            EXP_binary(
              op,
              (self#ctx_rewrite left ctx),
              (self#ctx_rewrite right ctx)
            )
          | EXP_if(cond_exp, then_exp, else_exp) ->
            EXP_if(
              (self#ctx_rewrite cond_exp ctx),
              (self#ctx_rewrite then_exp ctx),
              (self#ctx_rewrite else_exp ctx)
            )
          | EXP_let(vd, body_exp) ->
            let (id, ty, value_exp) = vd in
            EXP_let(
              (id,
               (self#rewrite_type ty),
               (self#ctx_rewrite value_exp ctx)),
              (self#ctx_rewrite body_exp ctx)
            )
          | EXP_let_rec(var_defs, body_exp ) ->
            let new_var_defs =
              var_defs |> List.map
                (fun vd -> let (id, ty, value_exp) = vd in
                  (id,
                   (self#rewrite_type ty),
                   (self#ctx_rewrite value_exp) ctx))
            in
            EXP_let_rec(
              new_var_defs,
              (self#ctx_rewrite body_exp ctx)
            )
          | EXP_func(arg_defs, ret_type, body_exp) ->
            let new_arg_defs = 
              (List.map
                 (fun arg_def ->
                    let (id, ty) = arg_def in
                    (id, (self#rewrite_type ty))
                 )
                 arg_defs
              ) in
            EXP_func(
              new_arg_defs,              
              (self#rewrite_type ret_type),
              (self#ctx_rewrite body_exp ctx)
            )
          | EXP_call(func_exp, arg_exps) -> EXP_call(
              (self#ctx_rewrite func_exp ctx),
              arg_exps |> List.map (fun e -> self#ctx_rewrite e ctx)
            )
      }

  method rewrite expr_node =
    self#ctx_rewrite expr_node self#default_ctx

end

class default_transform = object
  inherit [unit] expr_node_transform
  method private default_ctx = ()
end

let default_rewrite e =
  let xform = new default_transform in
  xform#rewrite e

