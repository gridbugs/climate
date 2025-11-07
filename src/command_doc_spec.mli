open! Import

(** Abstract representations of objects for the purpose of generating
    documentation *)

module Value : sig
  type t =
    { name : string
        (* The value name to be used in documentation, such as
           the "MESSAGE" in "--commit MESSAGE" *)
    ; required : bool
    }

  val pp : format_name:(string -> string) -> Format.formatter -> t -> unit
end

module Positional_arg : sig
  type t =
    { value : Value.t
    ; doc : string option
    }
end

module Positional_args : sig
  type t =
    { fixed : Positional_arg.t list
    ; repeated : Positional_arg.t option
    }
end

module Named_arg : sig
  type t =
    { names : Name.t Nonempty_list.t
    ; value : Value.t option
    ; repeated : bool
    ; default_string : string option
    ; doc : string option
    }
end

module Named_args : sig
  type t = Named_arg.t list
end

module Args : sig
  type t =
    { named : Named_args.t
    ; positional : Positional_args.t
    }

  val pp_usage_args
    :  format_positional_args:(string -> string)
    -> Format.formatter
    -> t
    -> unit
end

module Subcommand : sig
  type t =
    { name : Name.t
    ; aliases : Name.t list
    ; doc : string option
    ; args : Args.t
    }
end

module Subcommands : sig
  type t = Subcommand.t list
end

type t =
  { program_name : string
  ; subcommand : string list
  ; doc : string option
  ; args : Args.t
  ; subcommands : Subcommands.t
  }
