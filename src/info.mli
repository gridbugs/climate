open! Import

module Term : sig
  module Names : sig
    type t
  end

  module Locator : sig
    (** How to locate the term in an argument list *)
    type t =
      | Named of Names.t
      | At_position of Nonnegative_int.t
      | All_positional

    val named_of_strings : string list -> (t, Error.Spec.t) result
    val at_position_of_int : int -> (t, Error.Spec.t) result
  end

  type t

  val create : Locator.t -> t

  module Sealed : sig
    type info = t
    type t

    val create : info list -> (t, Error.Spec.t) result

    (* The resulting list is guaranteed to contain no duplicate names and no gaps in positional arguments *)
    val infos : t -> info list
    val infos_by_name : t -> info Name.Map.t
  end
end
