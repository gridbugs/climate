(* This program ignores its arguments and prints out a bash script.
   Put the script in a file and source it in your shell. You will find
   that the command "fib" now autocompletes to the Fibonacci sequence,
   adding a new number every time you press tab.

   dune exec examples/fib.exe > /tmp/completion.sh
   . /tmp/completion.sh
   fib <TAB> <TAB> <TAB> <TAB> ...
*)
open Climate

let () =
  let open Command in
  let command =
    singleton
      (let open Arg_parser in
       let+ program_name = program_name
       and+ _ =
         pos_all
           int
           ~completion:
             (Completion.reentrant
                (let+ all = pos_all int in
                 let x =
                   match List.rev all with
                   | [] -> 1
                   | [ a ] -> a
                   | a :: b :: _ -> a + b
                 in
                 [ x ]))
       in
       program_name)
  in
  let program_name = run command in
  print_endline
    (completion_script_bash command ~program_name:"fib" ~program_exe:program_name)
;;
