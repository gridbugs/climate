open Climate

(* A program with no subcommands and some flag arguments *)

let command =
  Command.singleton
    (let open Arg_parser in
     let+ _ = flag [ "foo" ]
     and+ _ = flag [ "baz" ]
     and+ _ = flag [ "bar" ] in
     ())
;;

let () =
  print_endline
    (Command.completion_script_bash
       command
       ~program_name:(Literal "basic")
       ~global_symbol_prefix:(`Custom "_basic_")
       ~command_hash_in_function_names:false)
;;
