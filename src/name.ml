open! Import

type t = string

module Invalid = struct
  type t =
    | Empty_name
    | Begins_with_dash
end

let of_string string =
  if String.is_empty string
  then Error Invalid.Empty_name
  else if String.starts_with string ~prefix:"-"
  then Error Invalid.Begins_with_dash
  else Ok string
;;

let of_string_exn string =
  match of_string string with
  | Ok t -> t
  | Error _ -> raise (Invalid_argument "Name.of_string_exn")
;;

let to_string t = t
let equal = String.equal

let kind t =
  match String.length t with
  | 1 -> `Short
  | n ->
    assert (n > 1);
    `Long
;;

let to_string_with_dashes t =
  let prefix =
    match kind t with
    | `Short -> "-"
    | `Long -> "--"
  in
  String.cat prefix t
;;

let chip_short_name_off_string string =
  if String.starts_with string ~prefix:"-"
  then Error Invalid.Begins_with_dash
  else (
    match String.length string with
    | 0 -> Error Invalid.Empty_name
    | 1 -> Ok (string, "")
    | n ->
      let name = String.get string 0 |> String.make 1 in
      let rest = String.sub string ~pos:1 ~len:(n - 1) in
      Ok (name, rest))
;;

module Set = Set.Make (String)
module Map = Map.Make (String)
