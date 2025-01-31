module Prose : sig
  type markup =
    [ `Paragraph of string
    | `Preformatted of string
    ]

  (** The parts of a manpage that are hand-written and not generated from the
      command line spec *)
  type t

  val empty : t

  val create
    :  ?description:markup list
    -> ?environment:markup list
    -> ?files:markup list
    -> ?examples:markup list
    -> ?authors:markup list
    -> unit
    -> t
end

type t =
  { prose : Prose.t
  ; help : Help.t
  ; version : string option
  }

val to_troff_string : t -> string
