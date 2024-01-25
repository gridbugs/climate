open Climate
open Term.O

let run = Util.eval_and_print_parse_error

let%expect_test "passing a value beginning with '-' to an argument" =
  let term =
    let+ value = Term.(opt_req [ "f"; "foo" ] string) in
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
  [%expect
    {|
      Option "-f" requires an argument but appears in a sequence of short names "-f--bar" in a non-final position. When passing multiple short names in a sequence only the final one may take an argument. |}];
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
    Option "-f" requires an argument but appears in a sequence of short names "-f-" in a non-final position. When passing multiple short names in a sequence only the final one may take an argument. |}]
;;
