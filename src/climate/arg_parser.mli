open! Import

(** A parser of values of type ['a] *)
type 'a t

type 'a parse = string -> ('a, [ `Msg of string ]) result
type 'a print = Format.formatter -> 'a -> unit

(** Make a ['a print] value from a to_string function *)
val to_string_print : ('a -> string) -> 'a print

module Completion : sig
  type 'a parser := 'a t
  type 'a t

  type command_line =
    { program : string
    ; subcommand : string list
    ; args : string list
    }

  (** Complete using paths relative to the current directory. This can be
      used with [conv]s of any type though care must be taken that the conv
      knows how to parse paths. This requirement isn't enforced with types
      as it would be too restrictive to be useful in general. *)
  val file : _ t

  val values : 'a list -> 'a t
  val reentrant : (command_line -> 'a list) -> 'a t
  val reentrant_parse : 'a list parser -> 'a t
  val reentrant_thunk : (unit -> 'a list) -> 'a t

  (** For use in cases the optionality of a value being
      parsed/completed needs to represented in the value itself. *)
  val some : 'a t -> 'a option t

  (** Flatten the completion information into plain strings. This can be used
      to supply completion hints that otherwise wouldn't satisfy the type
      constraints within a [conv], though care must be taken to ensure that
      the [conv] knows how to parse the suggestions as this will no longer be
      enforced by the type system when this function is used. *)
  val stringify : 'a t -> 'a print -> _ t
end

(** Knows how to interpret strings on the command line as a particular type
    and how to format values of said type as strings. Define a custom [_ conv]
    value to implement a parser for a custom type. *)
type 'a conv =
  { parse : 'a parse
  ; print : 'a print
  ; default_value_name : string
      (* In help messages, [default_value_name] is the placeholder for a value in
         the documentation of an argument with a parameter and in the usage
         message (e.g. "--foo=STRING"). *)
  ; completion : 'a Completion.t option
  }

(** Helper function for constructing ['_ conv]s *)
val make_conv
  :  parse:'a parse
  -> print:'a print
  -> ?default_value_name:string
  -> ?completion:'a Completion.t option
  -> unit
  -> 'a conv

val string : string conv
val int : int conv
val float : float conv
val bool : bool conv

(** Similar to [string] except its default value name and completion
    is specialized for files. *)
val file : string conv

(** [enum values ~eq] returns a conv for a concrete set of possible values of
    type ['a]. The values and their names are given by the [values] argument and
    [eq] is used when printing values to tie a given value of type ['a] to a
    name. *)
val enum
  :  ?default_value_name:string
  -> ?eq:('a -> 'a -> bool)
  -> (string * 'a) list
  -> 'a conv

(** [string_enum values ~eq] returns a conv for a concrete set of possible
    strings. *)
val string_enum : ?default_value_name:string -> string list -> string conv

(** [pair ~sep a b] returns a conv for a pair of values separated by
    the first occurance of [sep] (',' by default). *)
val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv

(** [pair ~sep a b] returns a conv for a list of values separated by
    [sep] (',' by default). *)
val list : ?sep:char -> 'a conv -> 'a list conv

val map : 'a t -> f:('a -> 'b) -> 'b t

(** Similar to [map] however [f] may return a [Non_ret.t]. *)
val map' : 'a t -> f:('a -> ('b, Non_ret.t) result) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

(** The apply operator in the parlance of applicative functors. *)
val apply : ('a -> 'b) t -> 'a t -> 'b t

(** Shorthand for [apply]. *)
val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

(** A parser that ignores the command line and always yields the same value *)
val const : 'a -> 'a t

(** A parser that takes no arguments and returns [()], included for testing purposes *)
val unit : unit t

(** A parser that resolves to the program name as it appeared on the
    command line. *)
val argv0 : string t

(** Takes a parser of a list and returns a parser that yields the
    last element of the list. Raises a [Parse_error.E] if the list
    is empty. *)
val last : 'a list t -> 'a t

(** A named argument that may appear multiple times on the command line. *)
val named_multi
  :  ?doc:string
  -> ?value_name:string
  -> ?hidden:bool
  -> ?completion:'a Completion.t
  -> string list
  -> 'a conv
  -> 'a list t

(** A named argument that may appear at most once on the command line. *)
val named_opt
  :  ?doc:string
  -> ?value_name:string
  -> ?hidden:bool
  -> ?completion:'a Completion.t
  -> string list
  -> 'a conv
  -> 'a option t

(** A named argument that may appear at most once on the command line. If the
    argument is not passed then a given default value will be used instead. *)
val named_with_default
  :  ?doc:string
  -> ?value_name:string
  -> ?hidden:bool
  -> ?completion:'a Completion.t
  -> string list
  -> 'a conv
  -> default:'a
  -> 'a t

(** A named argument that must appear exactly once on the command line. *)
val named_req
  :  ?doc:string
  -> ?value_name:string
  -> ?hidden:bool
  -> ?completion:'a Completion.t
  -> string list
  -> 'a conv
  -> 'a t

(** A flag that may appear multiple times on the command line.
    Evaluates to the number of times the flag appeared. *)
val flag_count : ?doc:string -> ?hidden:bool -> string list -> int t

(** A flag that may appear at most once on the command line. *)
val flag : ?doc:string -> string list -> bool t

(** [pos_opt i conv] declares an optional anonymous positional
    argument at position [i] (starting at 0). *)
val pos_opt
  :  ?doc:string
  -> ?value_name:string
  -> ?completion:'a Completion.t
  -> int
  -> 'a conv
  -> 'a option t

(** [pos_with_default i conv] declares an optional anonymous positional
    argument with a default value at position [i] (starting at 0). *)
val pos_with_default
  :  ?doc:string
  -> ?value_name:string
  -> ?completion:'a Completion.t
  -> int
  -> 'a conv
  -> default:'a
  -> 'a t

(** [pos_req i conv] declares a required anonymous positional
    argument at position [i] (starting at 0). *)
val pos_req
  :  ?doc:string
  -> ?value_name:string
  -> ?completion:'a Completion.t
  -> int
  -> 'a conv
  -> 'a t

(** Parses all positional arguments. *)
val pos_all
  :  ?doc:string
  -> ?value_name:string
  -> ?completion:'a Completion.t
  -> 'a conv
  -> 'a list t

(** [pos_left i conv] parses all positional arguments at positions less than
    i. *)
val pos_left
  :  ?doc:string
  -> ?value_name:string
  -> ?completion:'a Completion.t
  -> int
  -> 'a conv
  -> 'a list t

(** [pos_right i conv] parses all positional arguments at positions greater
    than i. *)
val pos_right
  :  ?doc:string
  -> ?value_name:string
  -> ?completion:'a Completion.t
  -> int
  -> 'a conv
  -> 'a list t

(** Stripped down versions of some functions from the parent module
    for use in reentrant completion functions. None of the
    documentation arguments are present as there is no access to
    documentation for the parsers for these functions. Parsers that
    would fail when passed multiple times no longer fail under this
    condition, since any errors encountered during autocompletion
    will be ignored, and it's more useful to have these functions do
    something rather than nothing. *)
module Reentrant : sig
  val unit : unit t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val named_multi : string list -> 'a conv -> 'a list t
  val named_opt : string list -> 'a conv -> 'a option t
  val named_with_default : string list -> 'a conv -> default:'a -> 'a t
  val flag_count : string list -> int t
  val flag : string list -> bool t
  val pos_opt : int -> 'a conv -> 'a option t
  val pos_all : 'a conv -> 'a list t
  val pos_left : int -> 'a conv -> 'a list t
  val pos_right : int -> 'a conv -> 'a list t
end

(** Low-level functions intended for use within this library only. *)
module Private : sig
  val spec : _ t -> Spec.t

  val finalize
    :  'a t
    -> doc:string option
    -> child_subcommands:Subcommand.t list
    -> prose:Manpage.Prose.t option
    -> 'a t

  (** A parser that just prints a usage message. *)
  val usage : _ t

  (** Like [named_opt] but takes its arguments as [Name.t]s rather than
      as strings is it's intended for use within this library. *)
  val named_opt_for_internal
    :  ?doc:string
    -> ?value_name:string
    -> ?hidden:bool
    -> ?completion:'a Completion.t
    -> Name.t Nonempty_list.t
    -> 'a conv
    -> allow_many:bool
    -> 'a option t

  val eval
    :  'a t
    -> command_line:Command_line.Rich.t
    -> ignore_errors:bool
    -> ('a, Non_ret.t) result
end
