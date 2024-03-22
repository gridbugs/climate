open Climate

let commit =
  let open Arg_parser in
  let+ _amend = flag [ "amend"; "a" ]
  and+ _bool = named_opt [ "bool"; "b" ] bool
  and+ _message = named_opt [ "m"; "message" ] string in
  ()
;;

let bisect_common =
  let open Arg_parser in
  let+ _foo = flag [ "foo" ]
  and+ _bar = flag [ "bar" ] in
  ()
;;

let () =
  let open Command in
  group
    [ subcommand "config" (singleton Arg_parser.unit)
    ; subcommand "commit" (singleton commit)
    ; subcommand
        "bisect"
        (group
           ~default_arg_parser:Arg_parser.unit
           [ subcommand "start" (singleton bisect_common)
           ; subcommand "replay" (singleton bisect_common)
           ])
    ; subcommand ~hidden:true "__internal" print_autocompletion_script_bash
    ]
  |> run
;;
