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
    ; hints : Hint.t list
    }
end

module Parser_spec : sig
  type t =
    { named_args : Named_arg.t list
    ; positional_args_hints : Hint.t list
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

val generate_bash : Spec.t -> program_name:string -> program_exe:string -> string

module Reentrant_query : sig
  val query_arg_name : Name.t
  val command_line_arg_name : Name.t
end
