open Stdlib.StdLabels
open Climate
open Term.O

let main =
  let+ args = Term.(pos_all int) in
  print_endline (Printf.sprintf "%d" (List.fold_left args ~init:0 ~f:( + )))
;;

let () = Command.singleton main |> Command.run
