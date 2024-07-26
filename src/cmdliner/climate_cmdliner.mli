(** A wrapper of climate that copies a subset of cmdliner's api
    allowing climate to be used as a drop-in replacement for cmdliner,
    mostly for the purpose of testing climate in real-world programs such
    as dune. *)

module Term : sig
  type 'a t

  val const : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
end

module Arg : sig
  type 'a t
  type 'a conv
  type info

  val ( & ) : ('a -> 'b) -> 'a -> 'b
  val value : 'a t -> 'a Term.t
  val info : ?docs:string -> ?docv:string -> ?doc:string -> string list -> info
  val some : ?none:string -> 'a conv -> 'a option conv
  val enum : (string * 'a) list -> 'a conv
  val opt : 'a conv -> 'a -> info -> 'a t
  val flag : info -> bool t
end

module Cmd : sig
  type 'a t
  type info

  val info : ?doc:string -> ?man:_ -> ?envs:_ -> ?version:_ -> string -> info
  val v : info -> 'a Term.t -> 'a t
  val group : ?default:'a Term.t -> info -> 'a t list -> 'a t
  val eval_value : ?catch:_ -> 'a t -> ('a, _) result
end
