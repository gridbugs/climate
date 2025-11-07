open! Import

type t =
  | Help of
      { command_doc_spec : Command_doc_spec.t
      ; error : bool
      ; message : string option
      }
  | Manpage of
      { prose : Manpage.Prose.t
      ; command_doc_spec : Command_doc_spec.t
      }
  | Parse_error of
      { error : Error.Parse_error.t
      ; command_doc_spec : Command_doc_spec.t
      }
  | Reentrant_query of { suggestions : string list }
  | Generate_completion_script of { completion_script : string }
