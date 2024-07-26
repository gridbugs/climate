open StdLabels

module Term = struct
  type 'a t = 'a Climate.Arg_parser.t

  let const = Climate.Arg_parser.const
  let app = Climate.Arg_parser.apply
  let ( $ ) = Climate.Arg_parser.apply
end

module Arg = struct
  type 'a t = 'a Climate.Arg_parser.t
  type 'a conv = 'a Climate.Arg_parser.conv

  type info =
    { desc : string option
    ; value_name : string option
    ; names : string list
    }

  let ( & ) f x = f x
  let value a = a
  let info ?docs:_ignored ?docv ?doc names = { desc = doc; value_name = docv; names }

  let opt conv default { desc; value_name; names } =
    Climate.Arg_parser.named_with_default ?desc ?value_name names conv ~default
  ;;

  let flag { desc; names; _ } = Climate.Arg_parser.flag ?desc names

  let some ?(none = "") (conv : 'a conv) =
    let parse string = Result.map Option.some (conv.parse string) in
    let print ppf v =
      match v with
      | None -> Format.pp_print_string ppf none
      | Some v -> conv.print ppf v
    in
    let completion = Option.map Climate.Arg_parser.Completion.some conv.completion in
    { Climate.Arg_parser.parse
    ; print
    ; default_value_name = conv.default_value_name
    ; completion
    }
  ;;

  let enum xs = Climate.Arg_parser.enum ~eq:( = ) xs
end

module Cmd = struct
  type 'a t =
    { name : string
    ; command : 'a Climate.Command.t
    }

  type info =
    { name : string
    ; doc : string option
    }

  let info ?doc ?man:_ ?envs:_ ?version:_ name = { name; doc }

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

  let eval_value ?catch:_ { command; _ } = Ok (Climate.Command.run command)
end
