val reentrant_autocompletion_query_name : Name.t

module Hint : sig
  type t =
    | File
    | Values of string list
    | Reentrant_index of int
end

module Named_arg : sig
  type t =
    { name : Name.t
    ; has_param : bool
    ; hint : Hint.t option
    }
end

module Parser_spec : sig
  type t =
    { named_args : Named_arg.t list
    ; positional_args_hint :
        Hint.t option (* TODO allow different hints for different positional args *)
    }

  val empty : t
end

module Spec : sig
  type t =
    { parser_spec : Parser_spec.t
    ; subcommands : subcommand list
    }

  and subcommand =
    { name : string
    ; spec : t
    }

  val empty : t
end

(** Returns the contents of a bash script for registering bash completion
    according to the given spec. [program_name] is the name that the bash
    completion rules will be registered under. [program_exe] is the executable
    to use when running reentrant queries. In production both arguments will
    typically just be the program's name, but while developing a program or
    testing a program that's not installed into PATH, it can be useful to
    separate the two names. *)
val generate_bash : Spec.t -> program_name:string -> program_exe:string -> string
