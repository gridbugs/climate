open! Import

module Term : sig
  type locator
  type info

  val named : string list -> locator
  val positional : int -> locator
  val all_positional : locator
  val info : locator -> info
  val ( & ) : ('a -> 'b) -> 'a -> 'b

  type 'a t

  val const : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val string : info -> string t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Command : sig
  type 'a t

  val singleton : 'a Term.t -> 'a t
  val run : 'a t -> 'a
end

module For_test = For_test
