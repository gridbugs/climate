open! Import

type t =
  { name : Name.t
  ; aliases : Name.t list
  ; doc : string option
  ; arg_spec : Spec.t
  }

let command_doc_spec { name; aliases; doc; arg_spec } =
  let args = Spec.command_doc_spec arg_spec in
  { Command_doc_spec.Subcommand.name; aliases; doc; args }
;;
