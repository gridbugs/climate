open! Import

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
    ; extra : (string * Markup.t list) list
    }

  let empty =
    { description = None
    ; environment = None
    ; files = None
    ; examples = None
    ; authors = None
    ; extra = []
    }
  ;;

  let create ?description ?environment ?files ?examples ?authors ?(extra = []) () =
    { description; environment; files; examples; authors; extra }
  ;;
end

type t =
  { prose : Prose.t
  ; spec : Command_doc_spec.t
  ; version : string option
  }

let header ~(spec : Command_doc_spec.t) ~version =
  let command_name = String.concat ~sep:"-" (spec.program_name :: spec.subcommand) in
  sprintf
    {|
.TH "%s" 1 "" "%s" "%s Manual"
|}
    (String.uppercase_ascii command_name)
    (sprintf
       "%s %s"
       (String.capitalize_ascii spec.program_name)
       (Option.value version ~default:""))
    (String.capitalize_ascii spec.program_name)
;;

let name ~(spec : Command_doc_spec.t) =
  let command_name = String.concat ~sep:"-" (spec.program_name :: spec.subcommand) in
  sprintf
    {|
.SH NAME
%s%s
|}
    command_name
    (Option.map spec.doc ~f:(sprintf " - %s") |> Option.value ~default:"")
;;

let commands (subcommands : Command_doc_spec.Subcommands.t) =
  ".SH COMMANDS\n"
  :: List.concat_map
       subcommands
       ~f:(fun { Command_doc_spec.Subcommand.name; doc; args } ->
         Command_doc_spec.Args.pp_usage_args
           ~format_positional_args:(fun name -> sprintf "\\fI%s\\fR" name)
           Format.str_formatter
           args;
         let usage_args = Format.flush_str_formatter () in
         [ Some (sprintf ".TP\n\\fB%s\\fR%s" (Name.to_string name) usage_args); doc ]
         |> List.filter_opt)
  |> String.concat ~sep:"\n"
;;

let named_arg_string (arg : Command_doc_spec.Named_arg.t) =
  let names =
    List.map (Nonempty_list.to_list arg.names) ~f:(fun name ->
      let string_name = Name.to_string_with_dashes name in
      match arg.value with
      | Some value ->
        let sep = if Name.is_short name then " " else "=" in
        sprintf "%s%s%s" string_name sep value.name
      | None -> string_name)
    |> String.concat ~sep:", "
  in
  match arg.default_string with
  | Some default_string -> sprintf "%s (default=%s)" names default_string
  | None -> names
;;

let options (args : Command_doc_spec.Named_args.t) =
  ".SH OPTIONS\n"
  :: List.concat_map args ~f:(fun args ->
    [ Some (sprintf ".TP\n%s" (named_arg_string args))
    ; Option.map args.doc ~f:(fun doc -> sprintf "%s" doc)
    ]
    |> List.filter_opt)
  |> String.concat ~sep:"\n"
;;

let to_troff_string { prose; spec; version } =
  let parts =
    ([ Some (header ~spec ~version)
     ; Some (name ~spec)
     ; Option.map prose.description ~f:(Markup.troff_block "DESCRIPTION")
     ; (match spec.subcommands with
        | [] -> None
        | subcommands -> Some (commands subcommands))
     ; (match spec.args.named with
        | [] -> None
        | subcommands -> Some (options subcommands))
     ; Option.map prose.environment ~f:(Markup.troff_block "ENVIRONMENT")
     ; Option.map prose.files ~f:(Markup.troff_block "FILES")
     ; Option.map prose.examples ~f:(Markup.troff_block "EXAMPLES")
     ; Option.map prose.authors ~f:(Markup.troff_block "AUTHORS")
     ]
     |> List.filter_opt)
    @ List.map prose.extra ~f:(fun (heading, markup) -> Markup.troff_block heading markup)
  in
  String.concat ~sep:"\n" parts
;;
