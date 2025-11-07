open! Import

type t = string

module Invalid = struct
  type t =
    | Empty_name
    | Begins_with_dash
    | Invalid_char of char
end

let invalid_chars = [ '=' ]
let find_invalid_char string = List.find_opt invalid_chars ~f:(String.contains string)
let is_invalid_char char = List.exists invalid_chars ~f:(( = ) char)

let check_invalid_char string =
  match find_invalid_char string with
  | Some invalid_char -> Error (Invalid.Invalid_char invalid_char)
  | None -> Ok ()
;;

let of_string string =
  let open Result.O in
  if String.is_empty string
  then Error Invalid.Empty_name
  else if String.starts_with string ~prefix:"-"
  then Error Begins_with_dash
  else
    let+ () = check_invalid_char string in
    string
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

let is_short t =
  match kind t with
  | `Short -> true
  | `Long -> false
;;

let is_long t =
  match kind t with
  | `Short -> false
  | `Long -> true
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
    | 1 ->
      let char = String.get string 0 in
      if is_invalid_char char then Error (Invalid_char char) else Ok (string, "")
    | n ->
      let char = String.get string 0 in
      if is_invalid_char char
      then Error (Invalid_char char)
      else (
        let name = String.make 1 char in
        let rest = String.sub string ~pos:1 ~len:(n - 1) in
        Ok (name, rest)))
;;

module Set = Set.Make (String)
module Map = Map.Make (String)
