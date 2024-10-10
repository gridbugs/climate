open! Import

module Hint : sig
  type 'reentrant t =
    | File
    | Values of string list
    | Reentrant of 'reentrant
end

module Named_arg : sig
  type 'reentrant t =
    { name : Name.t
    ; has_param : bool
    ; hint : 'reentrant Hint.t option
    }
end

module Positional_args_hints : sig
  type 'reentrant t =
    { finite_args : 'reentrant Hint.t option list
    ; repeated_arg : [ `No_hint | `Hint of 'reentrant Hint.t ] option
    }

  val empty : _ t
end

module Parser_spec : sig
  type 'reentrant t =
    { named_args : 'reentrant Named_arg.t list
    ; positional_args_hints : 'reentrant Positional_args_hints.t
    }

  val empty : _ t
end

(** A description of an entire CLI, with just enough information to
    generate a completion script. This is a separate type from the the
    spec types used for parsing in an attempt to simplify the logic for
    generating completion scripts (it's complicated enough as it is!).

    Also note the type parameter ['reentrant]. This is required for
    generating completion scripts capable of calling back into the
    program, allowing the program itself to compute completion
    suggestions. When running a CLI program, ['reentrant] will be a
    function type for computing suggestions, however when generating a
    completion script, ['reentrant] will be an integer index which
    uniquely identifies the function to call to compute
    suggestions. *)
type 'reentrant t =
  { parser_spec : 'reentrant Parser_spec.t
  ; subcommands : 'reentrant subcommand list
  }

and 'reentrant subcommand =
  { name : string
  ; spec : 'reentrant t
  }

val empty : _ t
val named_args_sorted : 'a t -> 'a Named_arg.t list

(** Returns all reentrant values in order of their reentrant query
    index. Reentrant query indices can be used as an index into this
    list to get the corresponding reentrant value. *)
val all_reentrants : 'reentrant t -> 'reentrant list

(** Replaces each reentrant value with its index. Indices are
    contiguous and start at 0. Each reentrant value will be replaced with
    the index at which it will appear in the list returned by
    [all_reentrants]. *)
val replace_reentrants_with_indices : _ t -> int t
