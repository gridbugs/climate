open Climate
module Parse_error = Error.Parse_error

let eval_and_print_parse_error (command : 'a Command.t) command_line =
  try
    let _ : 'a = Command.For_test.eval command command_line in
    ()
  with
  | Parse_error.E error -> print_endline (Parse_error.to_string error)
;;
