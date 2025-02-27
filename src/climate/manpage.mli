module Markup : sig
  type t =
    [ `P of string
    | `Pre of string
    ]
end

module Prose : sig
  (** The parts of a manpage that are hand-written and not generated from the
      command line spec *)
  type t

  val empty : t

  val create
    :  ?description:Markup.t list
    -> ?environment:Markup.t list
    -> ?files:Markup.t list
    -> ?examples:Markup.t list
    -> ?authors:Markup.t list
    -> unit
    -> t
end

type t =
  { prose : Prose.t
  ; help : Help.t
  ; version : string option
  }

val to_troff_string : t -> string
