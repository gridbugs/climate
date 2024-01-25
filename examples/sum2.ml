open Climate
open Term.O

let main =
  let+ a = Term.(pos_req 0 int)
  and+ b = Term.(pos_req 1 int) in
  print_endline (Printf.sprintf "%d" (a + b))
;;

let () = Command.singleton main |> Command.run
