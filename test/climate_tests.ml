module For_test = Climate.For_test

module Spec_errors = struct
  open For_test.Term

  let print_spec_error = function
    | Ok _ -> ()
    | Error e -> print_endline (For_test.Error.Spec.to_string e)

  let%expect_test "no names" =
    For_test.Info.Term.Locator.named_of_strings [] |> print_spec_error;
    [%expect {| At least one name must be provided. |}]

  let%expect_test "empty string name" =
    For_test.Info.Term.Locator.named_of_strings [ "" ] |> print_spec_error;
    [%expect {| The empty string cannot be used as a name. |}]

  let%expect_test "single name" =
    For_test.Info.Term.Locator.named_of_strings [ "foo" ] |> print_spec_error

  let%expect_test "duplicate names" =
    For_test.Info.Term.Locator.named_of_strings [ "foo"; "bar"; "foo" ]
    |> print_spec_error;
    [%expect {| The name "foo" appeared twice which is not allowed. |}]

  let%expect_test "positive positional argument" =
    For_test.Info.Term.Locator.at_position_of_int 1 |> print_spec_error

  let%expect_test "zero positional argument" =
    For_test.Info.Term.Locator.at_position_of_int 0 |> print_spec_error

  let%expect_test "negative positional argument" =
    For_test.Info.Term.Locator.at_position_of_int (-2) |> print_spec_error;
    [%expect {| The position -2 is negative which is not allowed. |}]

  let%expect_test "pair of terms with no overlap" =
    let term =
      let+ _ = string & info (named [ "foo"; "bar" ])
      and+ _ = string & info (named [ "baz"; "qux" ]) in
      ()
    in
    For_test.Term.sealed_infos term |> print_spec_error

  let%expect_test "pair of terms with overlap" =
    let term =
      let+ _ = string & info (named [ "foo"; "bar" ])
      and+ _ = string & info (named [ "bar"; "qux" ]) in
      ()
    in
    sealed_infos term |> print_spec_error;
    [%expect
      {| The name "bar" was duplicated across multiple terms appearing as part of the sets ["foo", "bar"] and ["bar", "qux"] |}]

  let%expect_test "range of positional arguments with no gaps" =
    let term =
      let+ _ = string & info (positional 2)
      and+ _ = string & info (positional 0)
      and+ _ = string & info (positional 1) in
      ()
    in
    sealed_infos term |> print_spec_error

  let%expect_test "range of positional arguments with gap" =
    let term =
      let+ _ = string & info (positional 2)
      and+ _ = string & info (positional 0) in
      ()
    in
    sealed_infos term |> print_spec_error;
    [%expect {| Gap in positional indices that will be parsed. The maximum positional index is 2 but there is no parser for the positional argument at index 1. |}]

  let%expect_test "range of positional arguments with gap and all_positional" =
    let term =
      let+ _ = string & info (positional 2)
      and+ _ = string & info (positional 0)
      and+ _ = string & info all_positional in
      ()
    in
    sealed_infos term |> print_spec_error
end
