open Climate

let term name =
  let open Arg_parser in
  let+ x = named_req [ "x" ] string
  and+ y = named_req [ "y" ] string in
  print_endline (Printf.sprintf "%s %s %s" name x y)
;;

let () =
  let open Command in
  group
    [ subcommand "foo" (singleton (term "foo"))
    ; subcommand
        "bar"
        (group
           ~default_arg_parser:(term "bar")
           [ subcommand "baz" (singleton (term "baz")) ])
    ]
  |> run
;;
