open! Import

module Style : sig
  type t =
    { program_doc : Ansi_style.t
    ; usage : Ansi_style.t
    ; arg_name : Ansi_style.t
    ; arg_doc : Ansi_style.t
    ; section_heading : Ansi_style.t
    ; error : Ansi_style.t
    }

  (** An opinionated default value with some colours and formatting *)
  val default : t

  (** Plain formatting for each part of help messages *)
  val plain : t
end

val pp_usage : Style.t -> Format.formatter -> Command_doc_spec.t -> unit
val pp : Style.t -> Format.formatter -> Command_doc_spec.t -> unit
