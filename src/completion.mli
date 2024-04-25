open! Import

val generate_bash
  :  _ Completion_spec.t
  -> program_name:string
  -> program_exe:string
  -> print_reentrant_completions_name:Name.t
  -> string
