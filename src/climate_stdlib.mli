module Result : sig
  include module type of Result

  val map : ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t
  val map_error : ('a, 'error1) t -> f:('error1 -> 'error2) -> ('a, 'error2) t
  val bind : ('a, 'error) t -> f:('a -> ('b, 'error) t) -> ('b, 'error) t

  module List : sig
    type ('a, 'error) t = ('a, 'error) result list

    val all : ('a, 'error) t -> ('a list, 'error) result
  end

  module O : sig
    val ( >>| ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
    val ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
    val ( let* ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
    val ( and+ ) : ('a, 'error) t -> ('b, 'error) t -> ('a * 'b, 'error) t
    val ( let+ ) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
  end
end

module Option : sig
  include module type of Option

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module List : sig
  include module type of StdLabels.List

  val find_duplicate : eq:('a -> 'a -> bool) -> 'a t -> 'a option
  val split_n : 'a t -> int -> 'a t * 'a t
end

module Map : sig
  include module type of MoreLabels.Map

  module type S = sig
    include S

    val find : 'a t -> key -> 'a option
    val set : 'a t -> key -> 'a -> 'a t
    val of_list : (key * 'a) list -> ('a t, key * 'a * 'a) Result.t
  end

  module Make (Key : OrderedType) : S with type key = Key.t
end

module Nonempty_list : sig
  type 'a t = ( :: ) of ('a * 'a list)

  val of_list : 'a list -> 'a t option
  val to_list : 'a t -> 'a list
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Nonnegative_int : sig
  type t

  val of_int : int -> t option
  val to_int : t -> int
end

module String : sig
  include module type of StdLabels.String

  val lsplit2 : t -> on:char -> (t * t) option
  val is_empty : t -> bool
  val drop_prefix : t -> prefix:t -> t option

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module Int : sig
  include module type of Int
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end
