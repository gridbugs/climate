open! Import

module Spec : sig
  type t =
    [ `Empty_name
    | `No_names
    | `Duplicate_name of Name.t
    | `Negative_position of int
    | `Duplicate_name_across_terms of Name.t * Name.t list * Name.t list
    | `Gap_in_positional_indices of [ `Gap of int ] * [ `Max of int ] ]

  val to_string : t -> string
  val result_get : ('a, t) result -> 'a
end
