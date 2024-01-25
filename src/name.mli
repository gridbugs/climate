open! Import

type t

module Invalid : sig
  type t =
    | Empty_name
    | Begins_with_dash
end

val of_string : string -> (t, Invalid.t) result
val of_string_exn : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val kind : t -> [ `Short | `Long ]
val to_string_with_dashes : t -> string

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
