open Climate

let main =
  let open Arg_parser in
  let+ a = pos_req 0 int
  and+ b = pos_req 1 int in
  print_endline (Printf.sprintf "%d" (a + b))
;;

let () = Command.singleton main |> Command.run
