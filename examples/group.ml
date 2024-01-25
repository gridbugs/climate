open Climate
open Term.O

let term name =
  let+ x = Term.(opt_req [ "x" ] string)
  and+ y = Term.(opt_req [ "y" ] string) in
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
