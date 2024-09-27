open StdLabels

module Manpage = struct
  type block =
    [ `S of string
    | `P of string
    | `Pre of string
    | `I of string * string
    | `Noblank
    | `Blocks of block list
    ]

  let s_name = "NAME"
  let s_synopsis = "SYNOPSIS"
  let s_description = "DESCRIPTION"
  let s_commands = "COMMANDS"
  let s_command_aliases = "COMMAND ALIASES"
  let s_arguments = "ARGUMENTS"
  let s_options = "OPTIONS"
  let s_common_options = "COMMON OPTIONS"
  let s_exit_status = "EXIT STATUS"
  let s_exit_status_intro = `P "$(iname) exits with:"
  let s_environment = "ENVIRONMENT"

  let s_environment_intro =
    `P "These environment variables affect the execution of $(tname):"
  ;;

  let s_files = "FILES"
  let s_examples = "EXAMPLES"
  let s_bugs = "BUGS"
  let s_authors = "AUTHORS"
  let s_see_also = "SEE ALSO"
  let s_none = "cmdliner-none"

  type title = string * int * string * string * string
  type t = title * block list

  type format =
    [ `Auto
    | `Pager
    | `Plain
    | `Groff
    ]

  let print _format _ppf _t = ()
end

module Term = struct
  type 'a t = 'a Climate.Arg_parser.t

  let const = Climate.Arg_parser.const
  let app f x = Climate.Arg_parser.apply f x
  let ( $ ) = app

  type 'a ret =
    [ `Ok of 'a
    | `Error of bool * string
    | `Help of Manpage.format * string option
    ]

  let ret t =
    Climate.Arg_parser.map t ~f:(function
      | `Ok x -> x
      | `Error (_, string) -> failwith string
      | `Help _ -> failwith "help")
  ;;
end

module Cmd = struct
  module Exit = struct
    type info =
      { doc : string option
      ; code : int
      }

    let info ?doc code = { doc; code }
  end

  module Env = struct
    type info = unit

    let info ?doc:_ _ = ()
  end

  type 'a t =
    { name : string
    ; command : 'a Climate.Command.t
    }

  type info =
    { name : string
    ; doc : string option
    }

  let info ?docs:_ ?doc ?man:_ ?envs:_ ?version:_ ?exits:_ name = { name; doc }
  let name (t : _ t) = t.name

  let v info term =
    let command = Climate.Command.singleton ?desc:info.doc term in
    { name = info.name; command }
  ;;

  let group ?default info cmds =
    let command =
      Climate.Command.group
        ?default_arg_parser:default
        ?desc:info.doc
        (List.map cmds ~f:(fun { name; command } ->
           Climate.Command.subcommand name command))
    in
    { name = info.name; command }
  ;;

  let print_completion_script_bash name =
    { name; command = Climate.Command.print_completion_script_bash }
  ;;

  let eval_value ?catch:_ { command; _ } = Ok (Climate.Command.run command)
end

module Arg = struct
  type 'a t = 'a Climate.Arg_parser.t
  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  type 'a printer = Format.formatter -> 'a -> unit
  type 'a conv = 'a parser * 'a printer

  let conv ?docv:_ (parser, printer) =
    let parser string =
      match parser string with
      | Ok x -> `Ok x
      | Error (`Msg message) -> `Error message
    in
    parser, printer
  ;;

  let conv' (parser, printer) =
    let parser string =
      match parser string with
      | Ok x -> `Ok x
      | Error message -> `Error message
    in
    parser, printer
  ;;

  let conv_parser = fst
  let conv_printer = snd

  type info =
    { desc : string option
    ; value_name : string option
    ; names : string list
    }

  let ( & ) f x = f x
  let value a = a

  let required t =
    let open Climate.Arg_parser in
    let+ value = t in
    match value with
    | Some x -> x
    | None -> failwith (Printf.sprintf "missing required argument")
  ;;

  let info ?docs:_ignored ?docv ?doc ?env:_ names =
    { desc = doc; value_name = docv; names }
  ;;

  let conv_to_climate (parse, print) =
    let parse string =
      match parse string with
      | `Ok x -> Ok x
      | `Error message -> Error (`Msg message)
    in
    let default_value_name = "VALUE" in
    let completion = None in
    { Climate.Arg_parser.parse; print; default_value_name; completion }
  ;;

  let conv_of_climate (conv : 'a Climate.Arg_parser.conv) : 'a conv =
    let parse string =
      match conv.parse string with
      | Ok x -> `Ok x
      | Error (`Msg message) -> `Error message
    in
    parse, conv.print
  ;;

  let opt conv default { desc; value_name; names } =
    Climate.Arg_parser.named_with_default
      ?desc
      ?value_name
      names
      (conv_to_climate conv)
      ~default
  ;;

  let flag { desc; names; _ } = Climate.Arg_parser.flag ?desc names

  let some ?(none = "") (conv : 'a conv) : 'a option conv =
    let conv = conv_to_climate conv in
    let parse string = Result.map Option.some (conv.parse string) in
    let print ppf v =
      match v with
      | None -> Format.pp_print_string ppf none
      | Some v -> conv.print ppf v
    in
    let completion = Option.map Climate.Arg_parser.Completion.some conv.completion in
    conv_of_climate
      { Climate.Arg_parser.parse
      ; print
      ; default_value_name = conv.default_value_name
      ; completion
      }
  ;;

  let enum xs = Climate.Arg_parser.enum ~eq:( = ) xs |> conv_of_climate
  let string = conv_of_climate Climate.Arg_parser.string
  let bool = conv_of_climate Climate.Arg_parser.bool
  let int = conv_of_climate Climate.Arg_parser.int
  let float = conv_of_climate Climate.Arg_parser.float

  (* TODO *)
  let dir = conv_of_climate Climate.Arg_parser.file
  let file = conv_of_climate Climate.Arg_parser.file

  let pos_all conv default { value_name; _ } =
    let open Climate.Arg_parser in
    let+ value = pos_all ?value_name (conv_to_climate conv) in
    match value with
    | [] -> default
    | xs -> xs
  ;;

  let pos_right i conv default { value_name; _ } =
    let open Climate.Arg_parser in
    let+ value = pos_right ?value_name i (conv_to_climate conv) in
    match value with
    | [] -> default
    | xs -> xs
  ;;

  let pos i conv default { value_name; _ } =
    let open Climate.Arg_parser in
    pos_with_default ?value_name i (conv_to_climate conv) ~default
  ;;

  let list ?sep conv =
    Climate.Arg_parser.list ?sep (conv_to_climate conv) |> conv_of_climate
  ;;

  let opt_all conv default { desc; names; value_name } =
    let open Climate.Arg_parser in
    let+ values = named_multi ?desc ?value_name names (conv_to_climate conv) in
    match values with
    | [] -> default
    | xs -> xs
  ;;

  let last = Climate.Arg_parser.last
  let man_format = Term.const `Auto

  let pair ?(sep = ',') a b =
    Climate.Arg_parser.pair ~sep (conv_to_climate a) (conv_to_climate b)
    |> conv_of_climate
  ;;

  let doc_alts_enum _ = "TODO"
end
