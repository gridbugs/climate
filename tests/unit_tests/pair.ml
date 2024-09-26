open Climate

let%expect_test "parsing a pair" =
  let parser =
    let open Arg_parser in
    let+ i, b = pos_req 0 (pair int bool) in
    Printf.printf "%d %b" i b
  in
  Command.(eval (singleton parser) [ "42,false" ]);
  [%expect {| 42 false |}]
;;
