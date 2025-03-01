module Help_style : sig
  type color =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    | `Bright_black
    | `Bright_red
    | `Bright_green
    | `Bright_yellow
    | `Bright_blue
    | `Bright_magenta
    | `Bright_cyan
    | `Bright_white
    ]

  type ansi_style =
    { bold : bool
    ; dim : bool
    ; underline : bool
    ; color : color option
    }

  val ansi_style_plain : ansi_style

  type t =
    { program_doc : ansi_style
    ; usage : ansi_style
    ; arg_name : ansi_style
    ; arg_doc : ansi_style
    ; section_heading : ansi_style
    }

  (** An opinionated default value with some colours and formatting *)
  val default : t

  (** Plain formatting for each part of help messages *)
  val plain : t
end

module Manpage : sig
  type markup =
    [ `P of string
    | `Pre of string
    ]

  (** The parts of a manpage that are hand-written and not generated from the
      command line spec *)
  type prose = Manpage.Prose.t

  val prose
    :  ?description:markup list
    -> ?environment:markup list
    -> ?files:markup list
    -> ?examples:markup list
    -> ?authors:markup list
    -> ?extra:(string * markup list) list
    -> unit
    -> prose
end

(** A DSL for declaratively describing a program's command-line arguments *)
module Arg_parser : sig
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

module Program_name : sig
  type t =
    | Argv0
    | Literal of string
end

module Command : sig
  type 'a t

  (** Declare a single command. *)
  val singleton : ?doc:string -> ?prose:Manpage.prose -> 'a Arg_parser.t -> 'a t

  type 'a subcommand

  val subcommand : ?hidden:bool -> ?aliases:string list -> string -> 'a t -> 'a subcommand

  (** [group children] returns a command with a hierarchy of subcommands, the
      leaves of which will be either singletons or empty groups (groups with an
      empty list of children). If the [default_arg_parser] argument is passed then
      sequences of subcommands may terminating with this command and will be
      passed with that argument. *)
  val group
    :  ?default_arg_parser:'a Arg_parser.t
    -> ?doc:string
    -> ?prose:Manpage.prose
    -> 'a subcommand list
    -> 'a t

  (** A command that has the side effect of printing the completion
      script of the entire command it's contained inside. It's safe to
      bury this inside a hidden command group of internal
      commands. The command takes some arguments to configure the
      generated completion script, similar to the arguments of the
      function [completion_script_bash]. *)
  val print_completion_script_bash : _ t

  (** Returns a bash script that can be sourced in a shell to register
      completions for the command.

      [program_name] will be the name which the completion script is
      registered under in the shell, and should be the main way users
      will call the program. Usually this should be the name of the
      executable that will be installed in the user's PATH.

      [program_exe_for_reentrant_query] determines how the completion script
      will call back into the program when resolving a reentrant query. Pass
      [`Program_name] (the default) to have the completion script run the
      value of the [program_name] argument. In order to experiment with
      completion scripts during development, pass [`Other "path/to/exe"]
      instead, to allow the completion script to find the development
      executable. For example one might pass
      [`Other "_build/default/bin/foo.exe"] if building with dune.

      [global_symbol_prefix] determines the prefix of global symbols in
      the generated script

      [command_hash_in_function_names] determines if function names are
      augmented with a hash of the subcommand path and argument (if
      applicable) for which the generated function computes completion
      suggestions. This helps to avoid cases where multiple functions
      are generated with the same names due to the names of subcommands
      to the program colliding with mangled names of generated
      functions. This should be rare in practice but to be safe this
      defaults to [true]. *)
  val completion_script_bash
    :  ?eval_config:Eval_config.t
    -> ?program_exe_for_reentrant_query:[ `Program_name | `Other of string ]
    -> ?global_symbol_prefix:[ `Random | `Custom of string ]
    -> ?command_hash_in_function_names:bool
    -> ?program_name:Program_name.t
    -> ?options:Completion.Options.t
    -> _ t
    -> string

  (** Run the command on given arguments. Raises a [Parse_error.E] if
      the command line is invalid. By default the program name will be
      taken from [Sys.argv.(0)] but this can be overriden via the
      [program_name] argument. *)
  val eval
    :  ?eval_config:Eval_config.t
    -> ?program_name:Program_name.t
    -> ?help_style:Help_style.t
    -> ?version:string
    -> 'a t
    -> string list
    -> 'a

  (** Run the command line parser returning its result. Parse errors are
      handled by printing an error message to stderr and exiting. *)
  val run
    :  ?eval_config:Eval_config.t
    -> ?program_name:Program_name.t
    -> ?help_style:Help_style.t
    -> ?version:string
    -> 'a t
    -> 'a

  (** [run_singleton arg_parser] is a shorthand for [run (singleton arg_parser)] *)
  val run_singleton
    :  ?eval_config:Eval_config.t
    -> ?program_name:Program_name.t
    -> ?help_style:Help_style.t
    -> ?version:string
    -> ?doc:string
    -> 'a Arg_parser.t
    -> 'a
end

module For_test : sig
  module Climate_stdlib : module type of Climate_stdlib
  module Non_ret : module type of Non_ret
  module Parse_error : module type of Error.Parse_error

  val eval_result
    :  program_name:string
    -> 'a Command.t
    -> string list
    -> ('a, Non_ret.t) result

  val print_help_spec : Command_doc_spec.t -> unit
  val print_manpage : Command_doc_spec.t -> Manpage.prose -> unit
end
