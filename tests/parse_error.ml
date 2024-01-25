open Climate
open Term.O

let eval_and_print_parse_error = Util.eval_and_print_parse_error

let%expect_test "no args to empty parser" =
  eval_and_print_parse_error (Command.singleton (Term.const ())) []
;;

let%expect_test "too many positional args" =
  eval_and_print_parse_error (Command.singleton (Term.const ())) [ "foo" ];
  [%expect
    {| Too many positional arguments. At most 0 positional arguments may be passed. |}]
;;

let%expect_test "unknown argument name" =
  eval_and_print_parse_error (Command.singleton (Term.const ())) [ "-x" ];
  [%expect {| Unknown argument name: -x |}]
;;

let%expect_test "missing required positional argument" =
  let term = Term.(pos_req 0 string) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [];
  [%expect {| Missing required positional argument at position 0. |}]
;;

let%expect_test "missing required named argument" =
  let term = Term.(opt_req [ "f"; "foo" ] string) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [];
  [%expect {| Missing required named argument: -f,--foo |}]
;;

let%expect_test "missing value for named argument" =
  let term = Term.(opt_req [ "f"; "foo" ] string) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "-f" ];
  [%expect {| Named argument "-f" lacks value. |}]
;;

let%expect_test "unexpected value for flag" =
  let term = Term.(flag [ "f"; "foo" ]) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "--foo=x" ];
  [%expect {| Flag --foo does not take an value but was passed "x" |}]
;;

let%expect_test "option used in non-final position of short sequence" =
  let term =
    let+ flag = Term.(flag [ "a" ])
    and+ value = Term.(opt_req [ "b" ] string) in
    print_endline (Printf.sprintf "%b %s" flag value)
  in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "-a"; "-b"; "foo" ];
  [%expect {| true foo |}];
  eval_and_print_parse_error command [ "-ab"; "foo" ];
  [%expect {| true foo |}];
  eval_and_print_parse_error command [ "-ba"; "foo" ];
  [%expect
    {|
    Option "-b" requires an argument but appears in a sequence of short names "-ba" in a non-final position. When passing multiple short names in a sequence only the final one may take an argument. |}]
;;

let%expect_test "short name used with --" =
  let term = Term.(flag [ "f"; "foo" ]) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "--f" ];
  [%expect
    {| Single-character names must only be specified with a single dash. "--f" is not allowed as it has two dashes. |}]
;;

let%expect_test "dash in short sequence" =
  let term = Term.(flag [ "f" ]) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "-f-" ];
  [%expect
    {| Encountered dash while parsing sequence of short names "-f-". Each character in a sequence of short names is interpreted as a short name, but dashes may not be used as short names. |}];
  eval_and_print_parse_error command [ "-ff-ff-" ];
  [%expect
    {| Encountered dash while parsing sequence of short names "-ff-ff-". Each character in a sequence of short names is interpreted as a short name, but dashes may not be used as short names. |}]
;;

let%expect_test "flag passed multiple times" =
  let term = Term.(flag [ "f" ]) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "-f" ];
  [%expect {| |}];
  eval_and_print_parse_error command [ "-ff" ];
  [%expect {| The flag "-f" was passed 2 times but must may only appear at most once. |}];
  eval_and_print_parse_error command [ "-f"; "-f"; "-f" ];
  [%expect {| The flag "-f" was passed 3 times but must may only appear at most once. |}]
;;

let%expect_test "required option passed multiple times" =
  let term = Term.(opt_req [ "f" ] string) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "-f"; "a"; "-f"; "b" ];
  [%expect {| The option "-f" was passed 2 times but must be passed exactly once. |}]
;;

let%expect_test "optional option passed multiple times" =
  let term = Term.(opt [ "f" ] string) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "-f"; "a"; "-f"; "b" ];
  [%expect {| The option "-f" was passed 2 times but may only appear at most once. |}]
;;

let%expect_test "incomplete subcommand" =
  let term = Term.const () in
  let command = Command.group [ "foo", Command.singleton term ] in
  eval_and_print_parse_error command [ "foo" ];
  [%expect {| |}];
  eval_and_print_parse_error command [];
  [%expect
    {| The command is incomplete. Additional subcommands are required to form a command. |}]
;;

let%expect_test "value conversion failed" =
  let term = Term.(opt [ "f" ] int) in
  let command = Command.singleton term in
  eval_and_print_parse_error command [ "-f"; "42" ];
  [%expect
    {| |}];
  eval_and_print_parse_error command [ "-f"; "x" ];
  [%expect
    {| Failed to parse the argument to "-f": invalid value: "x" (not an int) |}]
;;
