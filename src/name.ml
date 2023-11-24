open! Import

type t = string

module Invalid = struct
  type t = Empty_name | Begins_with_dash
end

let of_string string =
  if String.is_empty string then Error Invalid.Empty_name
  else if String.starts_with string ~prefix:"-" then
    Error Invalid.Begins_with_dash
  else Ok string

let of_string_exn string =
  match of_string string with
  | Ok t -> t
  | Error _ -> raise (Invalid_argument "Name.of_string_exn")

let to_string t = t
let equal = String.equal

let kind t =
  match String.length t with
  | 1 -> `Short
  | n ->
      assert (n > 1);
      `Long

let to_string_with_dashes t =
  let prefix = match kind t with `Short -> "-" | `Long -> "--" in
  String.cat prefix t

module Set = Set.Make (String)
module Map = Map.Make (String)
