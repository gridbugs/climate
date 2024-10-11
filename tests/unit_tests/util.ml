open Climate

let eval_and_print_parse_error (command : 'a Command.t) args =
  try
    let _ : 'a = Command.eval ~program_name:(Literal "foo.exe") command args in
    ()
  with
  | Parse_error.E error -> print_endline (Parse_error.to_string error)
;;
