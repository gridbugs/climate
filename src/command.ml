open! Import

type 'a t = Singleton of 'a Term.t

let singleton term = Singleton term

let run (Singleton term) =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let arg_table = Arg_table.create args in
  failwith "todo"
