open Climate
open Arg_parser

let check = Util.check_and_print_spec_error

let%expect_test "duplicate argument name" =
  check (fun () ->
    let+ (_ : bool) = flag [ "foo" ]
    and+ (_ : bool) = flag [ "foo" ] in
    ());
  [%expect {| The name "--foo" is used in multiple arguments. |}]
;;

let%expect_test "empty string argument name" =
  check (fun () -> flag [ "" ]);
  [%expect
    {| Attempted to use "" as an argument name. "" is not a valid argument name because it is the empty string which is not allowed. |}]
;;

let%expect_test "long name beginning with dash" =
  check (fun () -> flag [ "--foo" ]);
  [%expect
    {| Attempted to use "--foo" as an argument name. "--foo" is not a valid argument name because it begins with a dash which is not allowed. |}]
;;

let%expect_test "short name beginning with dash" =
  check (fun () -> flag [ "-" ]);
  [%expect
    {| Attempted to use "-" as an argument name. "-" is not a valid argument name because it begins with a dash which is not allowed. |}]
;;

let%expect_test "dash in name" =
  check (fun () -> flag [ "foo-bar" ]);
  [%expect {| |}]
;;

let%expect_test "equals sign in name" =
  check (fun () -> flag [ "foo=bar" ]);
  [%expect
    {| Attempted to use "foo=bar" as an argument name. "foo=bar" is not a valid argument name because it contains the character '=' which is not allowed. |}]
;;

let%expect_test "negative position" =
  check (fun () -> pos_req (-2) string);
  [%expect {| Attempted to declare positional argument with negative position: -2 |}]
;;

let%expect_test "duplicate enum names" =
  check (fun () ->
    pos_all
      (enum
         [ "one", 1; "two", 2; "three", 3; "one", 1 ]
         ~eq:Int.equal
         ~default_value_name:"VAL"));
  [%expect
    {| An enum was declared with duplicate names. The following names were duplicated: one |}]
;;

let%expect_test "gap in positional argument range" =
  check (fun () ->
    let+ (_ : string) = pos_req 0 string
    and+ (_ : string) = pos_req 2 string in
    ());
  [%expect
    {| Attempted to declare a parser with a gap in its positional arguments. No parser would interpret the argument at position 1 but there is a parser for at least one argument at a higher position. |}]
;;

let%expect_test "use of reserved help names" =
  check (fun () ->
    let+ (_ : string) = named_req [ "help" ] string in
    ());
  [%expect
    {| The name "--help" can't be used as it's reserved for printing help messages. |}]
;;
