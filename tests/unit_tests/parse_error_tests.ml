open Climate

let run = Util.eval_and_print_parse_error

let%expect_test "no args to empty parser" =
  run (Command.singleton (Arg_parser.const ())) []
;;

let%expect_test "too many positional args" =
  run (Command.singleton (Arg_parser.const ())) [ "foo" ];
  [%expect
    {| Too many positional arguments. At most 0 positional arguments may be passed. |}]
;;

let%expect_test "unknown argument name" =
  run (Command.singleton (Arg_parser.const ())) [ "-x" ];
  [%expect {| Unknown argument name: -x |}]
;;

let%expect_test "missing required positional argument" =
  let term = Arg_parser.(pos_req 0 string) in
  let command = Command.singleton term in
  run command [];
  [%expect {| Missing required positional argument at position 0. |}]
;;

let%expect_test "missing required named argument" =
  let term = Arg_parser.(named_req [ "f"; "foo" ] string) in
  let command = Command.singleton term in
  run command [];
  [%expect {| Missing required named argument: -f,--foo |}]
;;

let%expect_test "missing value for named argument" =
  let term = Arg_parser.(named_req [ "f"; "foo" ] string) in
  let command = Command.singleton term in
  run command [ "-f" ];
  [%expect {| Named argument "-f" lacks parameter. |}]
;;

let%expect_test "unexpected value for flag" =
  let term = Arg_parser.(flag [ "f"; "foo" ]) in
  let command = Command.singleton term in
  run command [ "--foo=x" ];
  [%expect {| Flag --foo does not take a parameter but was passed: "x" |}]
;;

let%expect_test "short name used with --" =
  let term = Arg_parser.(flag [ "f"; "foo" ]) in
  let command = Command.singleton term in
  run command [ "--f" ];
  [%expect
    {| Single-character names must only be specified with a single dash. "--f" is not allowed as it has two dashes but only one character. |}]
;;

let%expect_test "dash in short sequence" =
  let term = Arg_parser.(flag [ "f" ]) in
  let command = Command.singleton term in
  run command [ "-f-" ];
  [%expect
    {| Encountered dash while parsing sequence of short names "-f-". Each character in a sequence of short names is interpreted as a short name, but dashes may not be used as short names. |}];
  run command [ "-ff-ff-" ];
  [%expect
    {| Encountered dash while parsing sequence of short names "-ff-ff-". Each character in a sequence of short names is interpreted as a short name, but dashes may not be used as short names. |}]
;;

let%expect_test "flag passed multiple times" =
  let term = Arg_parser.(flag [ "f" ]) in
  let command = Command.singleton term in
  run command [ "-f" ];
  [%expect {| |}];
  run command [ "-ff" ];
  [%expect {| The flag "-f" was passed 2 times but must may only appear at most once. |}];
  run command [ "-f"; "-f"; "-f" ];
  [%expect {| The flag "-f" was passed 3 times but must may only appear at most once. |}]
;;

let%expect_test "required option passed multiple times" =
  let term = Arg_parser.(named_req [ "f" ] string) in
  let command = Command.singleton term in
  run command [ "-f"; "a"; "-f"; "b" ];
  [%expect {| The argument "-f" was passed 2 times but must be passed exactly once. |}]
;;

let%expect_test "optional option passed multiple times" =
  let term = Arg_parser.(named_opt [ "f" ] string) in
  let command = Command.singleton term in
  run command [ "-f"; "a"; "-f"; "b" ];
  [%expect {| The option "-f" was passed 2 times but may only appear at most once. |}]
;;

let%expect_test "value conversion failed" =
  let term = Arg_parser.(named_opt [ "f" ] int) in
  let command = Command.singleton term in
  run command [ "-f"; "42" ];
  [%expect {| |}];
  run command [ "-f"; "x" ];
  [%expect {| Failed to parse the argument to "-f": invalid value: "x" (not an int) |}]
;;

let%expect_test "equals sign in short sequence" =
  let term = Arg_parser.(named_opt [ "f" ] int) in
  let command = Command.singleton term in
  run command [ "-=f"; "42" ];
  [%expect {| Invalid character '=' in argument name "=" |}]
;;
