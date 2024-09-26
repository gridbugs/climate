open Climate

let run = Util.eval_and_print_parse_error

let%expect_test "parsing a pair" =
  let parser =
    let open Arg_parser in
    let+ i, b = pos_req 0 (pair int bool) in
    Printf.printf "%d %b" i b
  in
  run Command.(singleton parser) [ "42,false" ];
  [%expect {| 42 false |}]
;;

let%expect_test "parsing a sequence of separators" =
  let parser =
    let open Arg_parser in
    let+ a, b = pos_req 0 (pair string string) in
    Printf.printf "%s %s" a b
  in
  run Command.(singleton parser) [ ",,,,," ];
  [%expect {| ,,,, |}]
;;

let%expect_test "parsing the empty string" =
  let parser =
    let open Arg_parser in
    let+ a, b = pos_req 0 (pair string string) in
    Printf.printf "%s %s" a b
  in
  run Command.(singleton parser) [ "" ];
  [%expect {| Failed to parse the argument at position 0: No separator (,) found in "" |}]
;;

let%expect_test "parsing a single value with no separator" =
  let parser =
    let open Arg_parser in
    let+ a, b = pos_req 0 (pair string string) in
    Printf.printf "%s %s" a b
  in
  run Command.(singleton parser) [ "foo" ];
  [%expect
    {| Failed to parse the argument at position 0: No separator (,) found in "foo" |}]
;;

let%expect_test "parsing a separator on its own" =
  let parser =
    let open Arg_parser in
    let+ a, b = pos_req 0 (pair string string) in
    Printf.printf "%s %s" a b
  in
  run Command.(singleton parser) [ "," ];
  [%expect {| |}]
;;

let%expect_test "parsing a value that begins with a separator" =
  let parser =
    let open Arg_parser in
    let+ a, b = pos_req 0 (pair string string) in
    Printf.printf "%s %s" a b
  in
  run Command.(singleton parser) [ ",foo" ];
  [%expect {| foo |}]
;;

let%expect_test "parsing a value that ends with a separator" =
  let parser =
    let open Arg_parser in
    let+ a, b = pos_req 0 (pair string string) in
    Printf.printf "%s %s" a b
  in
  run Command.(singleton parser) [ "foo," ];
  [%expect {| foo |}]
;;

let%expect_test "parsing a pair containing a parse error" =
  let parser =
    let open Arg_parser in
    let+ i, b = pos_req 0 (pair int bool) in
    Printf.printf "%d %b" i b
  in
  run Command.(singleton parser) [ "42,foo" ];
  [%expect {| Failed to parse the argument at position 0: invalid value: "foo" (not an bool) |}]
;;
