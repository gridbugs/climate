open Import

module Markup = struct
  type t =
    [ `P of string
    | `Pre of string
    ]

  let to_troff_string = function
    | `P paragraph -> paragraph
    | `Pre pre -> sprintf ".nf\n%s\n.fi" pre
  ;;

  let troff_block heading ts =
    sprintf ".SH %s" heading :: List.map ts ~f:to_troff_string
    |> String.concat ~sep:"\n\n"
  ;;
end

module Prose = struct
  type t =
    { description : Markup.t list option
    ; environment : Markup.t list option
    ; files : Markup.t list option
    ; examples : Markup.t list option
    ; authors : Markup.t list option
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

let header ~(help : Help.t) ~version =
  let command_name = String.concat ~sep:"-" (help.program_name :: help.subcommand) in
  sprintf
    {|
.TH "%s" 1 "" "%s" "%s Manual"
|}
    (String.uppercase_ascii command_name)
    (sprintf
       "%s %s"
       (String.capitalize_ascii help.program_name)
       (Option.value version ~default:""))
    (String.capitalize_ascii help.program_name)
;;

let name ~(help : Help.t) =
  let command_name = String.concat ~sep:"-" (help.program_name :: help.subcommand) in
  sprintf
    {|
.SH NAME
%s%s
|}
    command_name
    (Option.map help.doc ~f:(sprintf " - %s") |> Option.value ~default:"")
;;

let to_troff_string { prose; help; version } =
  let parts =
    [ Some (header ~help ~version)
    ; Some (name ~help)
    ; Option.map prose.description ~f:(Markup.troff_block "DESCRIPTION")
    ; Option.map prose.environment ~f:(Markup.troff_block "ENVIRONMENT")
    ; Option.map prose.files ~f:(Markup.troff_block "FILES")
    ; Option.map prose.examples ~f:(Markup.troff_block "EXAMPLES")
    ; Option.map prose.authors ~f:(Markup.troff_block "AUTHORS")
    ]
    |> List.filter_opt
  in
  String.concat ~sep:"\n" parts
;;
