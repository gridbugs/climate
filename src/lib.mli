module Arg_parser = Arg_parser

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
    ; error : ansi_style
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

module Completion_options : sig
  type t =
    { no_comments : bool
    ; no_whitespace : bool
    ; minify_global_names : bool
    ; minify_local_variables : bool
    ; optimize_case_statements : bool
    }
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

  (** A command group with the same subcommands (transitively) as the parent of
      this command in the subcommand hierarchy, however each subcommand under
      this command just prints the usage string for that command. *)
  val help : _ t

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
    -> ?options:Completion_options.t
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
