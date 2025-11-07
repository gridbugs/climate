open! Import

type t =
  { name : Name.t
  ; aliases : Name.t list
  ; doc : string option
  ; arg_spec : Spec.t
  }

val command_doc_spec : t -> Command_doc_spec.Subcommand.t
