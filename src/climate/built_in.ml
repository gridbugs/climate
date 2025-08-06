open! Import

let help_long = Name.of_string_exn "help"
let help_names : _ Nonempty_list.t = [ help_long; Name.of_string_exn "h" ]
let manpage_names : _ Nonempty_list.t = [ Name.of_string_exn "manpage" ]
