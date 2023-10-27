open! Import

type t = string

let of_string string =
  if String.is_empty string then Error `Empty_name else Ok string

let to_string t = t
let equal = String.equal

module Set = Set.Make (String)
module Map = Map.Make (String)
