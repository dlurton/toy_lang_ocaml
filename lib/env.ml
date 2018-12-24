
open Types

(* An environment that has no variable and no parent. *)
let empty_env : env_t = fun _ -> None

(* Nests the specified env in another env that has a variable. *)
let extend_env env id value = fun search_id ->
  if id = search_id then
    Some(value)
  else
    env search_id

(*
   Nests the specified env in another env that has a variable that is backed by a lazily instantiated value.char_offset.
   This supports `let rec`.
*)
let extend_env_rec env id value_getter =
  let rec nested_env search_id =
    if id = search_id then
      (* TODO: cache result with Lazy? *)
       Some(value_getter nested_env)
     else
       env search_id
  in
  nested_env
