open Stdlib.StdLabels
open Climate

let main =
  let open Arg_parser in
  let+ args = pos_all int in
  print_endline (Printf.sprintf "%d" (List.fold_left args ~init:0 ~f:( + )))
;;

let () = Command.singleton main |> Command.run
