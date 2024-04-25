module Command_line : sig
  type t =
    { program : string
    ; args : string list
    }
end

(** A DSL for declaratively describing a program's command-line arguments *)
module Arg_parser : sig
  (** A parser of values of type ['a] *)
  type 'a t

  type 'a parse = string -> ('a, [ `Msg of string ]) result
  type 'a print = Format.formatter -> 'a -> unit

  module Completion : sig
    type 'a parser := 'a t
    type 'a t

    val file : string t
    val values : 'a list -> 'a t
    val reentrant_raw : (Command_line.t -> 'a list) -> 'a t
    val reentrant : 'a list parser -> 'a t
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
    -> (string * 'a) list
    -> eq:('a -> 'a -> bool)
    -> 'a conv

  (** [string_enum values ~eq] returns a conv for a concrete set of possible
      strings. *)
  val string_enum : ?default_value_name:string -> string list -> string conv

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** A parser that ignores the command line and always yields the same value *)
  val const : 'a -> 'a t

  (** A parser that takes no arguments and returns [()], included for testing purposes *)
  val unit : unit t

  (** A parser that resolves to the program's name. Specifically it
      resolves to the value of argv[0]. *)
  val program_name : string t

  (** A named argument that may appear multiple times on the command line. *)
  val named_multi
    :  ?desc:string
    -> ?value_name:string
    -> ?hidden:bool
    -> ?completion:'a Completion.t
    -> string list
    -> 'a conv
    -> 'a list t

  (** A named argument that may appear at most once on the command line. *)
  val named_opt
    :  ?desc:string
    -> ?value_name:string
    -> ?hidden:bool
    -> ?completion:'a Completion.t
    -> string list
    -> 'a conv
    -> 'a option t

  (** A named argument that may appear at most once on the command line. If the
      argument is not passed then a given default value will be used instead. *)
  val named_with_default
    :  ?desc:string
    -> ?value_name:string
    -> ?hidden:bool
    -> ?completion:'a Completion.t
    -> string list
    -> 'a conv
    -> default:'a
    -> 'a t

  (** A named argument that must appear exactly once on the command line. *)
  val named_req
    :  ?desc:string
    -> ?value_name:string
    -> ?hidden:bool
    -> ?completion:'a Completion.t
    -> string list
    -> 'a conv
    -> 'a t

  (** A flag that may appear multiple times on the command line.
      Evaluates to the number of times the flag appeared. *)
  val flag_count : ?desc:string -> ?hidden:bool -> string list -> int t

  (** A flag that may appear at most once on the command line. *)
  val flag : ?desc:string -> string list -> bool t

  (** [pos i conv] declares an optional anonymous positional argument at position
      [i] (starting at 0). *)
  val pos_opt
    :  ?value_name:string
    -> ?completion:'a Completion.t
    -> int
    -> 'a conv
    -> 'a option t

  (** [pos i conv] declares a required anonymous positional argument at position
      [i] (starting at 0). *)
  val pos_req
    :  ?value_name:string
    -> ?completion:'a Completion.t
    -> int
    -> 'a conv
    -> 'a t

  (** Parses all positional arguments. *)
  val pos_all : ?value_name:string -> ?completion:'a Completion.t -> 'a conv -> 'a list t

  (** [pos_left i conv] parses all positional arguments at positions less than
      i. *)
  val pos_left
    :  ?value_name:string
    -> ?completion:'a Completion.t
    -> int
    -> 'a conv
    -> 'a list t

  (** [pos_left i conv] parses all positional arguments at positions greater
      than or equal to i. *)
  val pos_right
    :  ?value_name:string
    -> ?completion:'a Completion.t
    -> int
    -> 'a conv
    -> 'a list t

  (** Stripped down versions of some functions from the parent module
      for use in reentrant completion functions. None of the
      documentation arguments are present as there is no access
      documentation for the parsers for these functions. Parsers that
      would fail when passed multiple times no longer fail under this
      condition, since any errors encountered during autocompletion
      will be ignored, and it's more useful to have these functions do
      something rather than nothing. *)
  module Reentrant : sig
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
end

module Eval_config : sig
  type t =
    { print_reentrant_completions_name : Name.t
    (** The name of a hidden named parameter that will be added to the
        top level command. It will be passed by the completion script
        to call user-provided functions for computing completion
        suggestions. *)
    }

  val default : t
end

module Command : sig
  type 'a t

  (** Declare a single command. Performs some checks that the parser is
      well-formed and raises a [Spec_error.E] if iat's invalid. *)
  val singleton : 'a Arg_parser.t -> 'a t

  type 'a subcommand

  val subcommand : ?hidden:bool -> string -> 'a t -> 'a subcommand

  (** [group children] returns a command with a hierarchy of subcommands, the
      leaves of which will be either singletons or empty groups (groups with an
      empty list of children). If the [default_arg_parser] argument is passed then
      sequences of subcommands may terminating with this command and will be
      passed with that argument. Performs some checks that each parser is
      well-formed and raises a [Spec_error.E] if an invalid parser is found.*)
  val group : ?default_arg_parser:'a Arg_parser.t -> 'a subcommand list -> 'a t

  (** A command that has the side effect of printing the completion
      script of the entire command it's contained inside. It's safe to
      bury this inside a hidden command group of internal commands. *)
  val print_completion_script_bash : _ t

  val completion_script_bash
    :  ?eval_config:Eval_config.t
    -> _ t
    -> program_name:string
    -> program_exe:string
    -> string

  (** Run the command line parser on a given list of terms. Raises a
      [Parse_error.E] if the command line is invalid. *)
  val eval : ?eval_config:Eval_config.t -> 'a t -> Command_line.t -> 'a

  (** Run the command line parser returning its result. Parse errors are
      handled by printing an error message to stderr and exiting. *)
  val run : ?eval_config:Eval_config.t -> 'a t -> 'a
end

module Parse_error : sig
  (** Errors encountered while interpreting command-line arguments. This
      indicates that the user of a CLI program made with this library has
      passed invalid command-line arguments to the program. *)
  type t

  exception E of t

  val to_string : t -> string
end

module Spec_error : sig
  (* Errors that indicate that a client of this library has attempted to
     create an invalid argument spec. *)
  type t

  exception E of t

  val to_string : t -> string
end

module For_test : module type of For_test
