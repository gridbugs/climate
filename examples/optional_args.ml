open Climate

let term =
  let open Arg_parser in
  let+ x = named_with_default [ "x" ] string ~default:"foo"
  and+ y = named_with_default [ "y" ] string ~default:"bar" in
  Printf.printf "%s and %s" x y
;;

let () = Command.singleton term |> Command.run
