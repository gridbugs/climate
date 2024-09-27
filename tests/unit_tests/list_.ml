open Climate

let run = Util.eval_and_print_parse_error

let%expect_test "parsing a list" =
  let parser =
    let open Arg_parser in
    let+ xs = pos_req 0 (list string) in
    List.iter print_endline xs
  in
  run Command.(singleton parser) [ "foo,bar,baz" ];
  [%expect {|
    foo
    bar
    baz
    |}]
;;

let%expect_test "parsing an empty string as a list" =
  let parser =
    let open Arg_parser in
    let+ xs = pos_req 0 (list string) in
    List.iter print_endline xs
  in
  run Command.(singleton parser) [ "" ];
  [%expect {| |}]
;;

let%expect_test "parsing a single value as a list" =
  let parser =
    let open Arg_parser in
    let+ xs = pos_req 0 (list string) in
    List.iter print_endline xs
  in
  run Command.(singleton parser) [ "foo" ];
  [%expect {| foo |}]
;;

let%expect_test "parsing a sequence of separators as a list" =
  let parser =
    let open Arg_parser in
    let+ xs = pos_req 0 (list string) in
    List.iter print_endline xs
  in
  run Command.(singleton parser) [ ",,,,,,," ];
  [%expect {| |}]
;;

let%expect_test "parsing a string begining with a separator as a list" =
  let parser =
    let open Arg_parser in
    let+ xs = pos_req 0 (list string) in
    List.iter print_endline xs
  in
  run Command.(singleton parser) [ ",foo" ];
  [%expect {| foo |}]
;;

let%expect_test "parsing a string ending with a separator as a list" =
  let parser =
    let open Arg_parser in
    let+ xs = pos_req 0 (list string) in
    List.iter print_endline xs
  in
  run Command.(singleton parser) [ "foo," ];
  [%expect {| foo |}]
;;

let%expect_test "parsing a list containing a parse error" =
  let parser =
    let open Arg_parser in
    let+ xs = pos_req 0 (list int) in
    List.iter print_int xs
  in
  run Command.(singleton parser) [ "1,two,3" ];
  [%expect
    {| Failed to parse the argument at position 0: invalid value: "two" (not an int) |}]
;;
