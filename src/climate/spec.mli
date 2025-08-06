open! Import

type untyped_completion_function = Command_line.Rich.t -> string list
type untyped_completion_hint = untyped_completion_function Completion_spec.Hint.t

module Named : sig
  module Info : sig
    (** A description of a single named argument *)
    type t =
      { names : Name.t Nonempty_list.t (** All the names that identify
                                           this argument *)
      ; has_param : [ `No | `Yes_with_value_name of string ]
      (** Whether this argument accepts a value. If it does accept a
          value, how should the value be referred to in help messages. *)
      ; default_string : string option
      (** Default value to display in documentation (if any) *)
      ; required : bool
      (** Just determines if argument is shown in
          usage string (not used for error checking) *)
      ; doc : string option (** Description to display in help messages *)
      ; completion : untyped_completion_hint option
      (** Completion hint for this argument *)
      ; hidden : bool (** If true, then this argument doesn't show up
                          in help messages. *)
      ; repeated : bool (** If true, this argument can be passed multiple times *)
      }

    (** Returns true iff the argument accepts a value *)
    val has_param : t -> bool
  end

  (** Spec for a set of named arguments *)
  type t

  (** True iff this spec has no named arguments. *)
  val is_empty : t -> bool

  (** Returns the [Info.t] for an argument with a given name *)
  val get_info_by_name : t -> Name.t -> Info.t option
end

module Positional : sig
  (** Spec for a set of positional arguments, keeping track of the
      index of each positional argument. It's possible for a parser to
      accept either a limited or unlimited number of positional
      arguments. If the parser accepts an unlimited number of
      arguments, all arguments above a certain index will have the
      same value name and completion hint. *)
  type t

  (** Returns the number of positional arguments described by the
      spec. *)
  val arg_count : t -> [ `Limited of int | `Unlimited ]

  (** True iff this spec has no positional arguments. *)
  val is_empty : t -> bool

  (** Create a spec with a single positional argument at a given
      index. *)
  val single_at_index
    :  int
    -> value_name:string
    -> required:bool
    -> completion:untyped_completion_hint option
    -> doc:string option
    -> t

  (** Create a spec with an unlimited number of positional arguments
      starting at a given index. These arguments cannot be required as
      there are an unlimited number of them. *)
  val all_above_inclusive
    :  int
    -> value_name:string
    -> completion:untyped_completion_hint option
    -> doc:string option
    -> t

  (** Create a spec with positional arguments from index 0 up to but
      not including some given index. *)
  val all_below_exclusive
    :  int
    -> value_name:string
    -> required:bool
    -> completion:untyped_completion_hint option
    -> doc:string option
    -> t
end

(** A description of all the arguments accepted by a CLI with enough
    information to parse a command line and generate a help message. *)
type t =
  { named : Named.t
  ; positional : Positional.t
  }

val empty : t

(** True iff this spec has no arguments. *)
val is_empty : t -> bool

(** [merge a b] returns a spec containing the union of arguments in
    [a] and [b]. Raises a [Error.Spec_error.E _] if [a] and [b] conflict,
    such as if they both contain an argument with the same name or
    position. *)
val merge : t -> t -> t

(** Create a spec with a single positional argument. *)
val create_positional : Positional.t -> t

(** Create a spec with a single named argument. *)
val create_named : Named.Info.t -> t

(** Helper for creating a spec with a single named argument which
    doesn't take a value (ie. a flag). *)
val create_flag
  :  Name.t Nonempty_list.t
  -> doc:string option
  -> hidden:bool
  -> repeated:bool
  -> t

val command_doc_spec : t -> Command_doc_spec.Args.t

val to_completion_parser_spec
  :  t
  -> untyped_completion_function Completion_spec.Parser_spec.t

(** Raises a [Error.Spec_error.E] if the spec is invalid *)
val validate : t -> unit
