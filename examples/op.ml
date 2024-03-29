open Stdlib.StdLabels
open Climate

let main =
  let open Arg_parser in
  let+ op = pos_req 0 string
  and+ args = pos_right 1 int in
  let init, op =
    match op with
    | "+" -> 0, ( + )
    | "*" -> 1, ( * )
    | other -> failwith (Printf.sprintf "unknown op %s" other)
  in
  print_endline (Printf.sprintf "%d" (List.fold_left args ~init ~f:op))
;;

let () = Command.singleton main |> Command.run
