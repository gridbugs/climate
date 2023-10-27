open! Import

type t

val of_string : string -> (t, [> `Empty_name ]) result
val to_string : t -> string
val equal : t -> t -> bool

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
