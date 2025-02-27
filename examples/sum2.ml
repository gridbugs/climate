open Climate

let main =
  let open Arg_parser in
  let+ a = pos_req 0 int ~value_name:"LHS" ~doc:"The left hand side of the operation"
  and+ b = pos_req 1 int ~value_name:"RHS" ~doc:"The right hand side of the operation" in
  print_endline (Printf.sprintf "%d" (a + b))
;;

let () = Command.singleton main |> Command.run
