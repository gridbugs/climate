open Climate

module Color = struct
  type t =
    | Red
    | Green
    | Blue

  (* Tell climate how to handle colours *)
  let conv =
    let open Arg_parser in
    enum ~default_value_name:"COLOR" [ "red", Red; "green", Green; "blue", Blue ]
  ;;
end

(* Ansi escape sequence to reset the termminal style *)
let ansi_reset = "\x1b[0m"

(* Returns the escape sequence to set the terminal style *)
let ansi_style ~bold ~underline ~color =
  let effects =
    List.append (if bold then [ ";1" ] else []) (if underline then [ ";4" ] else [])
  in
  let color_code =
    match (color : Color.t option) with
    | None -> 0
    | Some Red -> 31
    | Some Green -> 32
    | Some Blue -> 34
  in
  Printf.sprintf "\x1b[%d%sm" color_code (String.concat "" effects)
;;

(* Print the words in the given style *)
let main ~bold ~underline ~color words =
  print_string (ansi_style ~bold ~underline ~color);
  print_string (String.concat " " words);
  print_string ansi_reset;
  print_newline ()
;;

let () =
  let command =
    Command.singleton ~doc:"Echo with style!"
    @@
    let open Arg_parser in
    (* Describe and parse the command line arguments: *)
    let+ bold = flag [ "bold" ] ~doc:"Make the text bold"
    and+ underline = flag [ "underline" ] ~doc:"Underline the text"
    and+ color = named_opt [ "color" ] Color.conv ~doc:"Set the text color"
    and+ words = pos_all string
    and+ completion =
      flag [ "completion" ] ~doc:"Print this program's completion script and exit"
    in
    if completion
    then `Completion
    else `Main (fun () -> main ~bold ~underline ~color words)
  in
  (* Run the parser yielding either a main function to call or an indication
     that we should print the completion script. *)
  let help_style =
    let open Help_style in
    { program_doc = { ansi_style_plain with color = Some `Green }
    ; usage = { ansi_style_plain with color = Some `Yellow }
    ; section_heading = { ansi_style_plain with color = Some `Red }
    ; arg_name = { ansi_style_plain with color = Some `Blue }
    ; arg_doc = { ansi_style_plain with color = Some `Cyan }
    ; error = { ansi_style_plain with color = Some `Red }
    }
  in
  match
    Command.run ~program_name:(Literal "echo-ansi") ~version:"0.0.1" ~help_style command
  with
  | `Completion -> print_endline (Command.completion_script_bash command)
  | `Main main -> main ()
;;
