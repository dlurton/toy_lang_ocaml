open Types

(** Converts a Lexing.position to Types.source_location. *)
let src_loc_of_position (pos: Lexing.position) =
  make_src_loc
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

(**
   This is a convenience function for constructing an instance of Types.expr given a
   Types.expr_node and a Lexing.position.
 *)
let make_node exp (start_loc: Lexing.position) =
  { exp = exp; loc = src_loc_of_position start_loc; }


let find_first_index lst predicate =
  let rec find lst index =
    match lst with
    | [] -> None
    | hd::tl ->
      if predicate(hd) then
        Some(index)
      else
        find tl (index + 1)
  in
  find lst 0

