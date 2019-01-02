open Types

(*TODO: update these docs *)

 let dump_env env =
   let rec inner_dump_env ienv idepth =
     match ienv with
     | [] -> ()
     | hd::tl ->
       Printf.printf "%d: %s\n"
         idepth
         (String.concat " " (hd |> Array.map (fun i -> Pretty.pretty_string_of_value i) |> Array.to_list));
     inner_dump_env tl (idepth + 1)
   in
   print_string "Environment dump:\n";
   inner_dump_env env 0;
   flush stdout


let empty_env : env_t = []
let extend_env env value : env_t = value :: env

let apply_env env env_index var_index =
  (match List.nth_opt  env env_index with
   | None -> failwith (Printf.sprintf "env_index %d does not exist" env_index)
   | Some vars -> vars
  ).(var_index)


(* Evaluates the parsed expression with the specified top-level environment. *)
let eval e top_env : interp_result =
  let rec inner_eval (e: expr_node_t) (env: env_t) : value_t =
    match e.exp with
    | EXP_var id -> failwith ("Variable '" ^ id ^ "' still existed for some reason")
    | EXP_index (env_index, var_index, _) -> apply_env env env_index var_index
    | EXP_literal n -> n
    | EXP_logical(lop, left, right) ->
      begin
        let eval_bool expr = match inner_eval expr env with
          | VAL_bool b -> b
          | _ -> raise (InterpExn(expr.loc, ERR_logical_operand_not_bool))
        in
        match lop with 
        | LOP_and ->
          begin
            let lval = eval_bool left in
            (* short circuit; if left side is true do not evaluate right side *)
            if lval then
              VAL_bool(eval_bool right)
            else
              VAL_bool(false)
          end
        | LOP_or ->
          begin
            (* short circuit; if only evaluate right side if left side is false *)
            let lval = eval_bool left in
            if not lval then
              VAL_bool(eval_bool right)
            else
              VAL_bool(true)
          end
      end
    | EXP_binary(op, left, right) ->
      let values = ((inner_eval left env), (inner_eval right env)) in
      let binary_int_op (func: int * int -> value_t) =
        begin
          match values with
          (* we have integers on both sides -- perform addition *)
          | (VAL_int lval, VAL_int rval) -> func(lval, rval)
          | _ ->
            (* we have a non-integer somewhere *)
            raise (InterpExn(e.loc, ERR_arithmetic_on_non_number))
        end in
      begin
        match op with
        | OP_eq ->
          begin
            match values with
            | (VAL_int lval, VAL_int rval) -> VAL_bool(lval = rval)
            | (VAL_bool lval, VAL_bool rval) -> VAL_bool(lval = rval)
            | (_, _) -> VAL_bool(false)
          end
        | OP_add  -> binary_int_op (fun (l, r) -> VAL_int(l + r))
        | OP_sub  -> binary_int_op (fun (l, r) -> VAL_int(l - r))
        | OP_mul  -> binary_int_op (fun (l, r) -> VAL_int(l * r))
        | OP_div  -> binary_int_op (fun (l, r) ->
            if r = 0 then raise (InterpExn(e.loc, ERR_div_0)) else VAL_int(l / r))
        | OP_mod  -> binary_int_op (fun (l, r) ->
            if r = 0 then raise (InterpExn(e.loc, ERR_div_0)) else VAL_int(l mod r))
        | OP_gt   -> binary_int_op (fun (l, r) -> VAL_bool(l > r))
        | OP_gte  -> binary_int_op (fun (l, r) -> VAL_bool(l >= r))
        | OP_lt   -> binary_int_op (fun (l, r) -> VAL_bool(l < r))
        | OP_lte  -> binary_int_op (fun (l, r) -> VAL_bool(l <= r))
      end
    | EXP_if(cond_exp, then_exp, else_exp) ->
      let cond_val = inner_eval cond_exp env in
      begin
        match cond_val with
        | VAL_bool(true) -> inner_eval then_exp env
        | VAL_bool(false) -> inner_eval else_exp env
        | _ -> raise (InterpExn(cond_exp.loc, ERR_if_cond_not_bool))
      end
    | EXP_let(var_def, body_exp ) ->
        let (_, _, value_exp) = var_def in
        let the_value = inner_eval value_exp env in
        let nested_env = extend_env env [|the_value|] in
        inner_eval body_exp nested_env
    | EXP_let_rec(var_defs, body_exp) ->
      let future_vals = Array.make (List.length var_defs) (VAL_int(0)) in
      let nested_env = extend_env env future_vals in
        var_defs |> List.iteri
          (fun i vd ->
             let (_, _, value_exp) = vd in
             let value = inner_eval value_exp nested_env in
             Array.set future_vals i value
          );
        inner_eval body_exp nested_env
    | EXP_func(ids, _, body_exp) -> VAL_func((List.length ids), body_exp, env)
    | EXP_call(func_exp, arg_exps) ->
      let proc_val = inner_eval func_exp env in
      begin
        match proc_val with
        | VAL_func(expected_arg_count, body_exp, captured_env) ->
          let actual_arg_count = (List.length arg_exps) in
          if expected_arg_count <> actual_arg_count then
            raise (InterpExn(
                e.loc,
                (ERR_incorrect_arg_count(expected_arg_count, actual_arg_count))))
          else
            let arg_values = Array.of_list (List.map (fun e -> inner_eval e env) arg_exps)
            in
            let call_env = extend_env captured_env arg_values in
            inner_eval body_exp call_env
        | _ -> raise (InterpExn(e.loc, ERR_invoked_non_func))
      end
  in
  try
    let resolved_exp = e |> Resolve.resolve_rewrite in
    ignore (Check.type_of_exp resolved_exp);
    IR_success(inner_eval resolved_exp top_env)
  with InterpExn (loc, msg) ->
    IR_error(loc, msg)

(* Evaluates the parsed expression with an empty environment. *)
let eval_with_empty_env e =
  eval e empty_env

(* Uses the Menhir generated parser to turn a string into an AST. *)
let parse s =
  let lexbuf = Lexing.from_string(s) in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    PR_success(ast)
  with LexicalExn(src_loc, msg) ->
    PR_error(src_loc, "Lexical error: " ^ msg)
     | Parser.Error ->
       let sloc = Util.src_loc_of_position lexbuf.lex_curr_p in 
       PR_error(sloc, "Syntax error near this position")


