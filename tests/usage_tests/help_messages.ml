open Climate
open Command

let run command args =
  try eval ~program_name:(Literal "foo.exe") command args with
  | Usage -> ()
;;

let unit_command ?desc () = singleton ?desc Arg_parser.unit

let%expect_test "empty spec" =
  run (unit_command ()) [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTIONS]

    Options:
     --help, -h   Print help
    |}]
;;

let%expect_test "subcommands" =
  let command =
    group [ subcommand "foo" (unit_command ()); subcommand "bar" (unit_command ()) ]
  in
  run command [];
  [%expect
    {|
    Usage: foo.exe [OPTIONS]
           foo.exe [SUBCOMMAND]

    Options:
     --help, -h   Print help

    Subcommands:
     foo
     bar
    |}];
  run command [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTIONS]
           foo.exe [SUBCOMMAND]

    Options:
     --help, -h   Print help

    Subcommands:
     foo
     bar
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
    Usage: foo.exe [OPTIONS]
           foo.exe [SUBCOMMAND]

    program description

    Options:
     --help, -h   Print help

    Subcommands:
     foo  description of subcommand foo
     bar  description of subcommand bar
    |}];
  run command [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTIONS]
           foo.exe [SUBCOMMAND]

    program description

    Options:
     --help, -h   Print help

    Subcommands:
     foo  description of subcommand foo
     bar  description of subcommand bar
    |}];
  run command [ "foo" ];
  [%expect {||}];
  run command [ "foo"; "--help" ];
  [%expect
    {|
    Usage: foo.exe foo [OPTIONS]

    description of subcommand foo

    Options:
     --help, -h   Print help
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
    and+ _ = pos_req 1 string ~value_name:"ARG2"
    and+ _ = pos_req 0 (string_enum [ "x"; "y"; "z" ]) ~value_name:"XYZ" in
    ()
  in
  let command = singleton parser in
  run command [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTIONS] <XYZ> <ARG2>

    Options:
     --foo, -f <FILE>   description of foo
     --bar <ABC>   description of bar
     -c <INT>
     --help, -h   Print help
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
    Usage: foo.exe [OPTIONS]
           foo.exe [SUBCOMMAND]

    Fake version control software

    Options:
     --help, -h   Print help

    Subcommands:
     log  List recent commits
     commit  Make a new commit
    |}];
  run command [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTIONS]
           foo.exe [SUBCOMMAND]

    Fake version control software

    Options:
     --help, -h   Print help

    Subcommands:
     log  List recent commits
     commit  Make a new commit
    |}];
  run command [ "log"; "--help" ];
  [%expect
    {|
    Usage: foo.exe log [OPTIONS]

    List recent commits

    Options:
     --pretty, -p <VALUE>
     --help, -h   Print help
    |}];
  run command [ "commit"; "--help" ];
  [%expect
    {|
    Usage: foo.exe commit [OPTIONS]

    Make a new commit

    Options:
     --amend, -a
     -m, --message <STRING>
     --help, -h   Print help
    |}]
;;
