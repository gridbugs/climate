open! Import

type t

module Invalid : sig
  type t = Empty_name
end

val of_string : string -> (t, Invalid.t) result
val of_string_exn : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val kind : t -> [ `Short | `Long ]

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
