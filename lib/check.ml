
open Types

(*
how to determine the type of an expression:
   - literals have the type of the value contained within
   - Variable references have the type of the defined variable--requires environment lookup
   - if has the type of both branches, as long as they have the same type.  If they don't have the same type--error.
   - Binary epxressions:
     - with arithmetic operators have type integer.
     - with comparison operators have type bool.
   - Logical expressions have type bool.
   - let and let rec have the type of their body.
   - Function constructors have the type of their expressed return type
   - Function calls have the type of the function that they evaluate to.

type checks:
   - operands of operators +, -, /, %, >, >= <, <= must be int
   - operands of operators || and && must bool
   - values assigned in let and let rec must match the declared type
   - Arguments to function calls must match the declared parameter types.
   - The condition of the of the if statement must be a boolean value.
   - Both branches of an if statement must have the same type.
*)


(* Given a value_t, return its type_t. *)
let rec type_of_value v : type_t =
  match v with
  | VAL_bool _ -> TY_bool
  | VAL_int _ -> TY_int
    (* should the function type be included in the function value
       instead of having to interrogate body_node? *)
  | VAL_func (_, body_node, _) -> type_of_exp body_node
  | VAL_type _ -> failwith "TODO: remove VAL_type"

(* Given an expr_node_t, recursively perform type checks of its children
   and then return its type_t. *)
and type_of_exp node : type_t =
  let check_is_bool node =
    begin
      let actual_t = type_of_exp node in
      match actual_t with
      | TY_bool -> () (* do nothing, it's ok *)
      | _ -> raise (InterpExn(node.loc, ERR_expected_bool(actual_t)))
    end in
(* TODO: DRY *)
  let check_is_int node =
    begin
      let actual_t = type_of_exp node in
      match actual_t with
      | TY_int -> () (* do nothing, it's ok *)
      | _ -> raise (InterpExn(node.loc, ERR_expected_int(actual_t)))
    end
  in
  let check_types_equal n1 n2 blame_loc =
    let ty1 = type_of_exp n1 in
    let ty2 = type_of_exp n2 in
    if ty1 <> ty2 then
      raise (InterpExn(blame_loc, ERR_type_mismatch(ty1, ty2)))
  in
  let check_var_def var_def =
    let (_, expected_ty, value_exp) = var_def in
    let actual_ty = type_of_exp value_exp in
    if expected_ty <> actual_ty then
      raise (InterpExn(
          value_exp.loc,
          ERR_type_mismatch(expected_ty, actual_ty)))
  in
  match node.exp with
  | EXP_var _ -> failwith "Variable should have already been resolved"
  | EXP_index (_, _, var_type) -> var_type
  | EXP_literal n -> type_of_value n
  | EXP_logical(_, l_node, r_node) ->
    check_is_bool l_node;
    check_is_bool r_node;
    TY_bool
  | EXP_binary(op, l_node, r_node) ->
    begin
      match op with
      | OP_add | OP_sub | OP_mul | OP_div | OP_mod ->
        check_is_int l_node;
        check_is_int r_node;
        TY_int
      | OP_gt | OP_gte | OP_lt | OP_lte ->
        check_is_int l_node;
        check_is_int r_node;
        TY_bool
      | OP_eq ->
        check_types_equal l_node r_node node.loc;
        (* TODO: Do we care about function equality? What does that even mean? *)
        TY_bool
    end
  | EXP_if(cond_exp, then_exp, else_exp) ->
    check_is_bool cond_exp;
    let then_ty = type_of_exp then_exp in
    let else_ty = type_of_exp else_exp in
    if then_ty <> else_ty then
      raise (InterpExn(node.loc, ERR_if_branch_type_mismatch))
    else then_ty
  | EXP_let(vd, value_exp) ->
    check_var_def vd;
    type_of_exp value_exp
  | EXP_let_rec(var_defs, body_exp ) ->
    var_defs |> List.iter (fun vd -> check_var_def vd);
    type_of_exp body_exp
  | EXP_func(param_defs, ret_type, body_exp) ->
    let param_types = param_defs |> List.map (fun pd -> let (_, ty) = pd in ty) in
    let body_ty = type_of_exp body_exp in
    if ret_type <> body_ty then
      raise (InterpExn(body_exp.loc, ERR_type_mismatch(ret_type, body_ty)))
    else ();
    TY_func(param_types, ret_type)
  | EXP_call(func_exp, arg_exps) ->
    begin
      let func_ty = type_of_exp(func_exp) in
      match func_ty with
      | TY_func (param_types, ret_type) ->
        let param_count = List.length param_types in
        let arg_count = List.length arg_exps in
        if param_count <> arg_count then
          raise (InterpExn(node.loc, ERR_incorrect_arg_count(param_count, arg_count)))
        else
          arg_exps |> List.iteri (fun i arg_exp ->
              let param_ty = List.nth param_types i in
              let arg_ty = type_of_exp arg_exp in
              if arg_ty <> param_ty then
                raise (InterpExn(arg_exp.loc, ERR_arg_type_mismatch(i, param_ty, arg_ty)))
            );
        ret_type
      | _ -> raise (InterpExn(func_exp.loc, ERR_cannot_call_non_func))
    end
