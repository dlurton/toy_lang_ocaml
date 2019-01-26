

open Types
open Rewrite


type equation_t = type_t * type_t


class infer_transform = object(self)
  inherit [unit] expr_node_transform

  method private default_ctx = ()
end

