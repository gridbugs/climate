open! Import

module Raw : sig
  (** A raw command line. All words following the program name on the
      command line are treated as arguments (ie. there is no concept of
      subcommand). *)
  type t =
    { program : string
    ; args : string list
    }

  (** Read the command line from the environment. Raises an exception if
      the argument list is empty (there should always be at least one
      argument containing the program name). *)
  val from_env : unit -> t
end

module Rich : sig
  (** A command line where the subcommand has been separated from the
      raw argument list. *)
  type t =
    { program : string
    ; subcommand : string list
    ; args : string list
    }

  val to_raw : t -> Raw.t
end
