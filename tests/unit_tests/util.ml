open Climate

let eval_and_print_parse_error command args =
  match For_test.eval_result ~program_name:"foo.exe" command args with
  | Ok _ -> ()
  | Error (For_test.Non_ret.Parse_error error) ->
    print_endline (For_test.Parse_error.to_string error)
  | _ -> failwith "unexpected parser output"
;;
