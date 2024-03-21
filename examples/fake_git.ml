open Climate

let commit =
  let open Arg_parser in
  let+ _amend = flag [ "amend"; "a" ]
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
    [ Subcommand ("config", singleton Arg_parser.unit)
    ; Subcommand ("commit", singleton commit)
    ; Subcommand
        ( "bisect"
        , group
            ~default_arg_parser:Arg_parser.unit
            [ Subcommand ("start", singleton bisect_common)
            ; Subcommand ("replay", singleton bisect_common)
            ] )
    ; Hidden ("__internal", print_autocompletion_script_bash)
    ]
  |> run
;;
