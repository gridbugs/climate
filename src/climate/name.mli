open! Import

type t

module Invalid : sig
  type t =
    | Empty_name
    | Begins_with_dash
    | Invalid_char of char
end

val of_string : string -> (t, Invalid.t) result
val of_string_exn : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val kind : t -> [ `Short | `Long ]
val is_short : t -> bool
val is_long : t -> bool
val to_string_with_dashes : t -> string
val chip_short_name_off_string : string -> (t * string, Invalid.t) result

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
