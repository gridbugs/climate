open Climate

let term name =
  let open Arg_parser in
  let+ x = named_req [ "x" ] string
  and+ y = named_req [ "y" ] string in
  print_endline (Printf.sprintf "%s %s %s" name x y)
;;

let () =
  Command.group
    [ "foo", Command.singleton (term "foo")
    ; ( "bar"
      , Command.group ~default_term:(term "bar") [ "baz", Command.singleton (term "baz") ]
      )
    ]
  |> Command.run
;;
