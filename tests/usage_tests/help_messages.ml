open Climate
open Command

let run command args =
  try
    eval ~program_name:(Literal "foo.exe") ~help_style:Help_style.plain command args
  with
  | Usage -> ()
;;

let unit_command ?doc () = singleton ?doc Arg_parser.unit

let%expect_test "empty spec" =
  run (unit_command ()) [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}]
;;

let%expect_test "subcommands" =
  let command =
    group [ subcommand "foo" (unit_command ()); subcommand "bar" (unit_command ()) ]
  in
  run command [];
  [%expect
    {|
    Usage: foo.exe [COMMAND]
           foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      foo
      bar
    |}];
  run command [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [COMMAND]
           foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      foo
      bar
    |}]
;;

let%expect_test "descriptions" =
  let command =
    group
      ~doc:"program description"
      [ subcommand "foo" (unit_command ~doc:"description of subcommand foo" ())
      ; subcommand "bar" (unit_command ~doc:"description of subcommand bar" ())
      ]
  in
  run command [];
  [%expect
    {|
    program description

    Usage: foo.exe [COMMAND]
           foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      foo  description of subcommand foo
      bar  description of subcommand bar
    |}];
  run command [ "--help" ];
  [%expect
    {|
    program description

    Usage: foo.exe [COMMAND]
           foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      foo  description of subcommand foo
      bar  description of subcommand bar
    |}];
  run command [ "foo" ];
  [%expect {||}];
  run command [ "foo"; "--help" ];
  [%expect
    {|
    description of subcommand foo

    Usage: foo.exe foo [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}]
;;

let%expect_test "arguments" =
  let parser =
    let open Arg_parser in
    let+ _ = named_opt [ "foo"; "f" ] ~doc:"description of foo" file
    and+ _ =
      named_opt
        [ "bar" ]
        ~doc:"description of bar"
        (string_enum ~default_value_name:"ABC" [ "a"; "b"; "c" ])
    and+ _ = named_opt [ "c" ] int
    and+ _ = pos_req 1 string ~value_name:"ARG2" ~doc:"The second argument"
    and+ _ = pos_req 0 (string_enum [ "x"; "y"; "z" ]) ~value_name:"XYZ" in
    ()
  in
  let command = singleton parser in
  run command [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTION]… <XYZ> <ARG2>

    Arguments:
      <XYZ>
      <ARG2>  The second argument

    Options:
      -f, --foo <FILE>  description of foo
          --bar <ABC>   description of bar
      -c <INT>
      -h, --help        Show this help message.
    |}]
;;

let%expect_test "arguments and subcommands" =
  let log =
    singleton
      ~doc:"List recent commits"
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
      ~doc:"Make a new commit"
      (let open Arg_parser in
       let+ _amend = flag [ "amend"; "a" ]
       and+ _message = named_opt [ "m"; "message" ] string in
       ())
  in
  let command =
    group
      ~doc:"Fake version control software"
      [ subcommand "log" log; subcommand "commit" commit ]
  in
  run command [];
  [%expect
    {|
    Fake version control software

    Usage: foo.exe [COMMAND]
           foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      log     List recent commits
      commit  Make a new commit
    |}];
  run command [ "--help" ];
  [%expect
    {|
    Fake version control software

    Usage: foo.exe [COMMAND]
           foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      log     List recent commits
      commit  Make a new commit
    |}];
  run command [ "log"; "--help" ];
  [%expect
    {|
    List recent commits

    Usage: foo.exe log [OPTION]…

    Options:
      -p, --pretty <VALUE>
      -h, --help            Show this help message.
    |}];
  run command [ "commit"; "--help" ];
  [%expect
    {|
    Make a new commit

    Usage: foo.exe commit [OPTION]…

    Options:
      -a, --amend
      -m, --message <STRING>
      -h, --help              Show this help message.
    |}]
;;
