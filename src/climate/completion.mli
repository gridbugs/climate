open! Import

module Options : sig
  type t =
    { no_comments : bool
    ; minify_global_names : bool
    ; no_whitespace : bool
    }

  val default : t
end

(** Returns a bash script that can be sourced in a shell to register
    completions for the described completion spec.

    [program_name] is the name that will be registered for the
    completion script. The completions will only be used if this value
    is the first word of a command. It should be the name of the
    executable that ends up in PATH when the CLI tool is installed.

    [program_exe] is the executable that will be run when performing
    reentrant queries

    [print_reentrant_completions_name] is the name of the argument
    that will be passed to [program_exe] to make it print completion
    suggestions. This argument should take a value which is the
    integer index of the query.

    [global_symbol_prefix] determines the prefix of global symbols in
    the generated script

    [command_hash_in_function_names] determines if function names are
    augmented with a hash of the subcommand path and argument (if
    applicable) for which the generated function computes completion
    suggestions. This helps to avoid cases where multiple functions
    are generated with the same names due to the names of subcommands
    to the program colliding with mangled names of generated
    functions. This should be rare in practice. *)
val generate_bash
  :  _ Completion_spec.t
  -> program_name:string
  -> program_exe_for_reentrant_query:[ `Program_name | `Other of string ]
  -> print_reentrant_completions_name:Name.t
  -> global_symbol_prefix:[ `Random | `Custom of string ]
  -> command_hash_in_function_names:bool
  -> options:Options.t
  -> string
