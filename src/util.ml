open Types

(** Converts a Lexing.position to Types.source_location. *)
let source_location_of_position (pos: Lexing.position) =
  make_source_location
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

(**
   This is a convenience function for constructing an instance of Types.expr given a
   Types.expr_node and a Lexing.position.
 *)
let make_node exp (start_loc: Lexing.position) =
  { exp = exp; loc = source_location_of_position start_loc; }


