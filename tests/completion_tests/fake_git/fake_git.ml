open Climate

let ls dir =
  let dir_handle = Unix.opendir dir in
  let rec loop files =
    try
      let file = Unix.readdir dir_handle in
      loop (file :: files)
    with
    | End_of_file -> files
  in
  List.rev (loop [])
;;

(* Fake protocol for defining branches so the list of branches can be
   updated dynamically by tests. *)
let list_branches () =
  ls "./.git/branches"
  |> List.filter (Fun.negate @@ String.starts_with ~prefix:".")
  |> List.sort String.compare
;;

let branch_conv =
  let open Arg_parser in
  { string with
    default_value_name = "BRANCH"
  ; completion = Some (Completion.reentrant_thunk list_branches)
  }
;;

(* A command which mixes reentrant completion with file completion *)
let checkout =
  let open Arg_parser in
  let+ _create_branch = flag [ "b" ]
  and+ _branch_name = pos_req 0 branch_conv
  and+ _files = pos_right 0 file in
  ()
;;

(* A command which mixes reentrant completion with a named argument
   with a value completed with an enum *)
let log =
  let open Arg_parser in
  let+ _pretty =
    named_opt [ "pretty"; "p" ] (string_enum [ "full"; "fuller"; "short"; "oneline" ])
  and+ _branch_name = pos_req 0 branch_conv in
  ()
;;

(* A command which mixes positional arguments and subcommands *)
let bisect op =
  let open Arg_parser in
  match op with
  | `Mark ->
    let+ _status = pos_opt 0 (string_enum [ "good"; "bad" ]) in
    ()
  | `Start | `Reset -> unit
;;

let () =
  let open Command in
  group
    [ subcommand "checkout" (singleton checkout)
    ; subcommand "log" (singleton log)
    ; subcommand "bisect"
      @@ group
           ~default_arg_parser:(bisect `Mark)
           [ subcommand "start" (singleton (bisect `Start))
           ; subcommand "reset" (singleton (bisect `Reset))
           ]
    ; subcommand "completions" ~hidden:true print_completion_script_bash
    ]
  |> run
;;
