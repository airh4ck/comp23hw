open Ast

val find_identifiers_pattern : pattern -> (id, Base.String.comparator_witness) Base.Set.t
val find_identifiers : expression -> (id, Base.String.comparator_witness) Base.Set.t
