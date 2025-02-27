open Import

module Prose = struct
  type markup =
    [ `Paragraph of string
    | `Preformatted of string
    ]

  type t =
    { description : markup list option
    ; environment : markup list option
    ; files : markup list option
    ; examples : markup list option
    ; authors : markup list option
    }

  let empty =
    { description = None
    ; environment = None
    ; files = None
    ; examples = None
    ; authors = None
    }
  ;;

  let create ?description ?environment ?files ?examples ?authors () =
    { description; environment; files; examples; authors }
  ;;
end

type t =
  { prose : Prose.t
  ; help : Help.t
  ; version : string option
  }

let to_troff_string { prose; help; version } =
  let _ = prose in
  let command_name = String.concat ~sep:"-" (help.program_name :: help.subcommand) in
  sprintf
    {|
.TH "%s" 1 "" "%s" "%s Manual"
.SH NAME
%s%s
|}
    (String.uppercase_ascii command_name)
    (sprintf
       "%s %s"
       (String.capitalize_ascii help.program_name)
       (Option.value version ~default:""))
    (String.capitalize_ascii help.program_name)
    command_name
    (Option.map help.doc ~f:(sprintf " - %s") |> Option.value ~default:"")
;;
