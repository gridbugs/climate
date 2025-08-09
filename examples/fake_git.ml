(* Git-like program to exercise completion *)
open Climate

let branch_conv =
  let open Arg_parser in
  { string with
    default_value_name = "BRANCH"
  ; completion =
      Some
        (Completion.reentrant_parse
           ((* This is supposed to simulate passing an alternative
               root directory to git, and having it be respected when
               invoking git while generating completion suggestions. *)
            let+ _root = named_opt [ "root" ] file in
            [ "main"; "devel" ]))
  }
;;

let checkout =
  let open Arg_parser in
  (* Multiple different completions for positional arguments *)
  let+ _branch = pos_req 0 (string_enum [ "foo"; "bar" ])
  and+ _ = pos_req 1 file
  and+ _ = pos_right 1 branch_conv in
  ()
;;

let commit =
  let open Arg_parser in
  let+ _amend = flag [ "amend" ] ~doc:"Amend a commit"
  and+ _all = flag [ "a" ] ~doc:"Commit all changes"
  and+ _branch = named_opt [ "b"; "branch" ] branch_conv
  and+ _message =
    named_opt
      [ "m"; "message" ]
      string
      ~doc:
        "The commit message. This description is extra long to exercise text wrapping in \
         help messages."
  and+ _files = pos_all file ~doc:"The files to commit" in
  ()
;;

let log =
  let open Arg_parser in
  let+ _pretty =
    named_opt
      [ "pretty"; "p" ]
      (string_enum [ "full"; "fuller"; "short"; "oneline" ])
      ~doc:"foo"
  in
  ()
;;

let bisect_common =
  let open Arg_parser in
  (* Mixing subcommands and positional arguments *)
  let+ _foo = named_opt [ "foo" ] int
  and+ _bar = flag [ "bar" ]
  and+ _baz = pos_opt 0 (string_enum [ "x"; "y"; "z" ]) in
  ()
;;

let () =
  let open Command in
  group
    ~prose:
      (Manpage.prose
         ~description:
           [ `P
               "An imitation of a version control tool with subcommands similar to those \
                of git."
           ]
         ())
    ~doc:"Fake version control"
    [ subcommand "config" (singleton Arg_parser.unit ~doc:"Configure the tool.")
    ; subcommand "checkout" (singleton checkout ~doc:"Check out a revision.")
    ; subcommand "commit" (singleton commit ~doc:"Commit your changes.")
    ; subcommand "log" ~aliases:[ "l" ] (singleton log ~doc:"List recent commits.")
    ; subcommand
        "bisect"
        (group
           ~doc:"Binary search through previous commits."
           [ subcommand "start" (singleton bisect_common ~doc:"Start a bisect.")
           ; subcommand "reset" (singleton bisect_common ~doc:"Stop a bisect.")
           ; subcommand "help" help
           ])
    ; subcommand ~hidden:true "__internal" print_completion_script_bash
    ; subcommand "help" help
    ]
  |> run ~program_name:(Literal "fake-git") ~version:"0.1.0"
;;
