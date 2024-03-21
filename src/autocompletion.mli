module Arg : sig
  type t =
    { name : string
    ; has_param : bool
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
