open Import

module Style : sig
  type t =
    { name : Ansi_style.t
    ; heading : Ansi_style.t
    }

  val default : t
end

type 'name entry =
  { name : 'name
  ; desc : string option
  }

module Value : sig
  type t =
    { name : string
    ; required : bool
    }
end

module Positional_args : sig
  type name = Value.t
  type nonrec entry = name entry

  type t =
    { fixed : entry list
    ; repeated : entry option
    }
end

module Named_args : sig
  type name =
    { names : Name.t Nonempty_list.t
    ; value : Value.t option
    ; repeated : bool
    }

  type nonrec entry = name entry
  type t = entry list
end

module Subcommands : sig
  type name = Name.t
  type nonrec entry = name entry
  type t = entry list
end

module Arg_sections : sig
  type t =
    { positional_args : Positional_args.t
    ; named_args : Named_args.t
    }
end

module Sections : sig
  type t =
    { arg_sections : Arg_sections.t
    ; subcommands : Subcommands.t
    }
end

type t =
  { program_name : string
  ; subcommand : string list
  ; desc : string option
  ; sections : Sections.t
  }

val pp : Style.t -> Format.formatter -> t -> unit
