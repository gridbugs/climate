open Climate

let run = Util.eval_and_print_parse_error

let%expect_test "passing a value beginning with '-' to an argument" =
  let term =
    let open Arg_parser in
    let+ value = named_req [ "f"; "foo" ] string
    and+ (_ : string option) = named_opt [ "b"; "bar" ] string in
    print_endline value
  in
  let command = Command.singleton term in
  run command [ "--foo"; "--bar" ];
  [%expect {| --bar |}];
  run command [ "--foo=--bar" ];
  [%expect {| --bar |}];
  run command [ "-f"; "--bar" ];
  [%expect {| --bar |}];
  run command [ "-f--bar" ];
  [%expect {|
      --bar |}];
  run command [ "--foo"; "--" ];
  [%expect {| -- |}];
  run command [ "--foo=--" ];
  [%expect {| -- |}];
  run command [ "--foo"; "--"; "--" ];
  [%expect {| -- |}];
  run command [ "--foo"; "-" ];
  [%expect {| - |}];
  run command [ "--foo=-" ];
  [%expect {| - |}];
  run command [ "-f"; "-" ];
  [%expect {| - |}];
  run command [ "-f-" ];
  [%expect {|
    - |}];
  run command [ "--foo"; "--bar"; "--bar"; "--foo" ];
  [%expect {| --bar |}]
;;

let%expect_test "passing equals sign to argument" =
  let term =
    let open Arg_parser in
    let+ value = named_req [ "f"; "foo" ] string in
    print_endline value
  in
  let command = Command.singleton term in
  run command [ "--foo====" ];
  [%expect {| === |}];
  run command [ "-f===" ];
  [%expect {| === |}];
  run command [ "--foo"; "===" ];
  [%expect {| === |}];
  run command [ "-f"; "===" ];
  [%expect {| === |}]
;;
