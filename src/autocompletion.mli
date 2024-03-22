module Hint : sig
  type t =
    | File
    | Values of string list
end

module Arg : sig
  type t =
    { name : Name.t
    ; has_param : bool
    ; hint : Hint.t option
    }
end

module Spec : sig
  type t =
    { args : Arg.t list
    ; subcommands : subcommand list
    }

  and subcommand =
    { name : string
    ; spec : t
    }

  val empty : t
end

val generate_bash : Spec.t -> program_name:string -> string
