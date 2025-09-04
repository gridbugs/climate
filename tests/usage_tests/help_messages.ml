open Climate
open Command

let run command args =
  match For_test.eval_result ~program_name:"foo.exe" command args with
  | Ok () -> ()
  | Error (For_test.Non_ret.Help { command_doc_spec; _ }) ->
    For_test.print_help_spec command_doc_spec
  | Error (For_test.Non_ret.Manpage { prose; command_doc_spec }) ->
    For_test.print_manpage command_doc_spec prose
  | _ -> failwith "unexpected parser output"
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
      [ subcommand
          "foo"
          ~aliases:[ "f" ]
          (unit_command ~doc:"description of subcommand foo" ())
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
      foo, f  description of subcommand foo
      bar     description of subcommand bar
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
      foo, f  description of subcommand foo
      bar     description of subcommand bar
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
  |}];
  run command [ "--manpage" ];
  [%expect
    {|
    .TH "FOO.EXE" 1 "" "Foo.exe " "Foo.exe Manual"


    .SH NAME
    foo.exe - program description

    .SH COMMANDS

    .TP
    \fBfoo\fR [\fIOPTION\fR]…
    description of subcommand foo

    Aliases: f
    .TP
    \fBbar\fR [\fIOPTION\fR]…
    description of subcommand bar
    .SH OPTIONS

    .TP
    --help, -h
    Show this help message.
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

let%expect_test "help command" =
  let command =
    group
      [ subcommand "foo" (unit_command ())
      ; subcommand
          "bar"
          (group
             [ subcommand "baz" (unit_command ())
             ; subcommand "help" help
               (* exercise placing a help command somewhere other than the top level *)
             ])
      ; subcommand "help" help
      ]
  in
  (* Test that the help command shows up in the list of commands printed by
     running with --help. *)
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
      help  Print documentation for a subcommand.
    |}];
  (* Test that running the help command with no arguments prints the help
     message. *)
  run command [ "help" ];
  [%expect
    {|
    Usage: foo.exe help [COMMAND]
           foo.exe help [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      foo
      bar
      help  Print documentation for a subcommand.
    |}];
  (* Test that we can pass a subcommand to the help command and it will print
     the help message for that subcommand. *)
  run command [ "help"; "foo" ];
  [%expect
    {|
    Usage: foo.exe foo [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}];
  (* Test that we can pass a subcommand with subcommands of its own to the help
     command and it will print the help message for that subcommand including
     its subcommands. *)
  run command [ "help"; "bar" ];
  [%expect
    {|
    Usage: foo.exe bar [COMMAND]
           foo.exe bar [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      baz
      help  Print documentation for a subcommand.
    |}];
  (* Test that we can pass a subcommand path to the help command. *)
  run command [ "help"; "bar"; "baz" ];
  [%expect
    {|
    Usage: foo.exe bar baz [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}];
  (* Test the behaviour when the help command is deeper in the subcommand
     hierarchy. *)
  run command [ "bar"; "help" ];
  [%expect
    {|
    Usage: foo.exe bar help [COMMAND]
           foo.exe bar help [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      baz
      help  Print documentation for a subcommand.
    |}];
  (* Test the behaviour when the help command is passed to itself. This is
     different from the output of passing --help to the help command for
     technical reasons. Fixing it isn't a high priority at the moment.. *)
  run command [ "help"; "help" ];
  [%expect
    {|
    Subcommand help. Pass the name of a subcommand for info.

    Usage: foo.exe help [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}];
  (* Test that we can pass the path to a second help command to a help command. *)
  run command [ "help"; "bar"; "help" ];
  [%expect
    {|
    Subcommand help. Pass the name of a subcommand for info.

    Usage: foo.exe bar help [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}];
  (* Test the help message for the help command. *)
  run command [ "help"; "--help" ];
  [%expect
    {|
    Subcommand help. Pass the name of a subcommand (see below) for info.

    Usage: foo.exe help [COMMAND]
           foo.exe help [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      foo
      bar
      help  Print documentation for a subcommand.
    |}];
  (* Test the behaviour passing --help to a subcommand of a help command. This
     should print the help message for the base help command rather than the
     subcommand. *)
  run command [ "help"; "foo"; "--help" ];
  [%expect
    {|
    Subcommand help. Pass the name of a subcommand (see below) for info.

    Usage: foo.exe help [COMMAND]
           foo.exe help [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      foo
      bar
      help  Print documentation for a subcommand.
    |}];
  (* Test passing --help to a help command that isn't in the root command. *)
  run command [ "bar"; "help"; "--help" ];
  [%expect
    {|
    Subcommand help. Pass the name of a subcommand (see below) for info.

    Usage: foo.exe bar help [COMMAND]
           foo.exe bar help [OPTION]…

    Options:
      -h, --help  Show this help message.

    Commands:
      baz
      help  Print documentation for a subcommand.
    |}]
;;

let%expect_test "entire application is help command" =
  (* This should never happen but check that it doesn't crash. *)
  let command = help in
  (* Run the command with no args. *)
  run command [];
  [%expect
    {|
    Usage: foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}];
  (* Run the command with --help. *)
  run command [ "--help" ];
  [%expect
    {|
    Subcommand help. Pass the name of a subcommand (see below) for info.

    Usage: foo.exe [OPTION]…

    Options:
      -h, --help  Show this help message.
    |}]
;;

let%expect_test "positional arguments" =
  run
    (singleton
     @@
     let open Arg_parser in
     let+ _ = pos_req 0 string ~value_name:"FIRST" ~doc:"first doc"
     and+ _ = pos_req 1 string ~value_name:"SECOND" ~doc:"second doc"
     and+ _ = pos_right 1 string ~value_name:"REST" ~doc:"rest doc" in
     ())
    [ "--help" ];
  [%expect
    {|
    Usage: foo.exe [OPTION]… <FIRST> <SECOND> [REST]…

    Arguments:
      [REST]...  rest doc
      <FIRST>    first doc
      <SECOND>   second doc

    Options:
      -h, --help  Show this help message.
    |}]
;;
