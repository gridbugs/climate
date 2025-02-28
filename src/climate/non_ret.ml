open! Import

type t =
  | Help of Command_doc_spec.t
  | Manpage of
      { spec : Command_doc_spec.t
      ; prose : Manpage.Prose.t
      }
  | Reentrant_query of { suggestions : string list }
  | Parse_error of Error.Parse_error.t
  | Generate_completion_script of { completion_script : string }
