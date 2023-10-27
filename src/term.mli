open! Import

type locator = Info.Term.Locator.t
type info = Info.Term.t

val named : string list -> locator
val positional : int -> locator
val all_positional : locator
val info : locator -> info
val ( & ) : ('a -> 'b) -> 'a -> 'b

type 'a t

val sealed_infos : _ t -> (Info.Term.Sealed.t, Error.Spec.t) result
val const : 'a -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
val string : Info.Term.t -> string t
