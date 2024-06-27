open! Import

module Raw = struct
  type t =
    { program : string
    ; args : string list
    }

  let from_env () =
    match Sys.argv |> Array.to_list with
    | program :: args -> { program; args }
    | [] -> failwith "unable to read command-line arguments from environment"
  ;;
end

module Rich = struct
  type t =
    { program : string
    ; subcommand : string list
    ; args : string list
    }

  let to_raw { program; subcommand; args } = { Raw.program; args = subcommand @ args }
end
