open Climate
open Command

let run command args =
  try
    eval ~program_name:(Literal "foo.exe") ~help_style:Help_style.plain command args
  with
  | Usage -> ()
;;

let unit_command ?desc () = singleton ?desc Arg_parser.unit

let%expect_test "empty spec" =
  run (unit_command ()) [ "--help" ];
  [%expect
    {|
    [33;1mUsage: [0m[32;1mfoo.exe [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help
    |}]
;;

let%expect_test "subcommands" =
  let command =
    group [ subcommand "foo" (unit_command ()); subcommand "bar" (unit_command ()) ]
  in
  run command [];
  [%expect
    {|
    [33;1mUsage: [0m[32;1mfoo.exe [COMMAND]
           foo.exe [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help

    [33;1mCommands:[0m
      [32;1mfoo [0m
      [32;1mbar [0m
    |}];
  run command [ "--help" ];
  [%expect
    {|
    [33;1mUsage: [0m[32;1mfoo.exe [COMMAND]
           foo.exe [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help

    [33;1mCommands:[0m
      [32;1mfoo [0m
      [32;1mbar [0m
    |}]
;;

let%expect_test "descriptions" =
  let command =
    group
      ~desc:"program description"
      [ subcommand "foo" (unit_command ~desc:"description of subcommand foo" ())
      ; subcommand "bar" (unit_command ~desc:"description of subcommand bar" ())
      ]
  in
  run command [];
  [%expect
    {|
    program description

    [33;1mUsage: [0m[32;1mfoo.exe [COMMAND]
           foo.exe [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help

    [33;1mCommands:[0m
      [32;1mfoo [0m description of subcommand foo
      [32;1mbar [0m description of subcommand bar
    |}];
  run command [ "--help" ];
  [%expect
    {|
    program description

    [33;1mUsage: [0m[32;1mfoo.exe [COMMAND]
           foo.exe [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help

    [33;1mCommands:[0m
      [32;1mfoo [0m description of subcommand foo
      [32;1mbar [0m description of subcommand bar
    |}];
  run command [ "foo" ];
  [%expect {||}];
  run command [ "foo"; "--help" ];
  [%expect
    {|
    description of subcommand foo

    [33;1mUsage: [0m[32;1mfoo.exe foo [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help
    |}]
;;

let%expect_test "arguments" =
  let parser =
    let open Arg_parser in
    let+ _ = named_opt [ "foo"; "f" ] ~desc:"description of foo" file
    and+ _ =
      named_opt
        [ "bar" ]
        ~desc:"description of bar"
        (string_enum ~default_value_name:"ABC" [ "a"; "b"; "c" ])
    and+ _ = named_opt [ "c" ] int
    and+ _ = pos_req 1 string ~value_name:"ARG2" ~desc:"The second argument"
    and+ _ = pos_req 0 (string_enum [ "x"; "y"; "z" ]) ~value_name:"XYZ" in
    ()
  in
  let command = singleton parser in
  run command [ "--help" ];
  [%expect
    {|
    [33;1mUsage: [0m[32;1mfoo.exe [OPTIONS] <XYZ> <ARG2>[0m

    [33;1mArguments:[0m
      [32;1m<XYZ> [0m
      [32;1m<ARG2> [0m The second argument

    [33;1mOptions:[0m
      [32;1m-f, --foo <FILE> [0m description of foo
      [32;1m    --bar <ABC> [0m  description of bar
      [32;1m-c <INT> [0m
      [32;1m-h, --help [0m       Print help
    |}]
;;

let%expect_test "arguments and subcommands" =
  let log =
    singleton
      ~desc:"List recent commits"
      (let open Arg_parser in
       let+ _pretty =
         named_opt
           [ "pretty"; "p" ]
           (string_enum [ "full"; "fuller"; "short"; "oneline" ])
       in
       ())
  in
  let commit =
    singleton
      ~desc:"Make a new commit"
      (let open Arg_parser in
       let+ _amend = flag [ "amend"; "a" ]
       and+ _message = named_opt [ "m"; "message" ] string in
       ())
  in
  let command =
    group
      ~desc:"Fake version control software"
      [ subcommand "log" log; subcommand "commit" commit ]
  in
  run command [];
  [%expect
    {|
    Fake version control software

    [33;1mUsage: [0m[32;1mfoo.exe [COMMAND]
           foo.exe [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help

    [33;1mCommands:[0m
      [32;1mlog [0m    List recent commits
      [32;1mcommit [0m Make a new commit
    |}];
  run command [ "--help" ];
  [%expect
    {|
    Fake version control software

    [33;1mUsage: [0m[32;1mfoo.exe [COMMAND]
           foo.exe [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-h, --help [0m Print help

    [33;1mCommands:[0m
      [32;1mlog [0m    List recent commits
      [32;1mcommit [0m Make a new commit
    |}];
  run command [ "log"; "--help" ];
  [%expect
    {|
    List recent commits

    [33;1mUsage: [0m[32;1mfoo.exe log [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-p, --pretty <VALUE> [0m
      [32;1m-h, --help [0m           Print help
    |}];
  run command [ "commit"; "--help" ];
  [%expect
    {|
    Make a new commit

    [33;1mUsage: [0m[32;1mfoo.exe commit [OPTIONS][0m

    [33;1mOptions:[0m
      [32;1m-a, --amend [0m
      [32;1m-m, --message <STRING> [0m
      [32;1m-h, --help [0m             Print help
    |}]
;;
