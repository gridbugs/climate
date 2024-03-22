open Climate

let branch_conv =
  let open Arg_parser in
  { string with
    default_value_name = "BRANCH"
  ; autocompletion_hint = Some (Reentrant (fun _command_line -> [ "main"; "devel" ]))
  }
;;

let checkout =
  let open Arg_parser in
  let+ _branch = pos_req 0 branch_conv in
  ()
;;

let commit =
  let open Arg_parser in
  let+ _amend = flag [ "amend"; "a" ]
  and+ _message = named_opt [ "m"; "message" ] string in
  ()
;;

let log =
  let open Arg_parser in
  let+ _pretty =
    named_opt [ "pretty"; "p" ] (string_enum [ "full"; "fuller"; "short"; "oneline" ])
  in
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
    ; subcommand "checkout" (singleton checkout)
    ; subcommand "commit" (singleton commit)
    ; subcommand "log" (singleton log)
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
