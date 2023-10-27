let term =
  let open Climate.Term in
  let+ foo = const "foo"
  and+ bar = const 42
  and+ some_string = string & info (named [ "some-string"; "s" ]) in
  print_endline (Printf.sprintf "%s %d %s" foo bar some_string)

let () = Climate.Command.(singleton term |> run)
