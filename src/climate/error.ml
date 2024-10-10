open! Import

let names_to_string_hum names =
  Nonempty_list.to_list names
  |> List.map ~f:Name.to_string_with_dashes
  |> String.concat ~sep:","
;;

module Parse_error = struct
  type t =
    | Arg_lacks_param of Name.t
    | Flag_has_param of
        { name : Name.t
        ; value : string
        }
    | No_such_arg of Name.t
    | Name_would_begin_with_dash of string
    | Short_name_would_be_dash of { entire_short_sequence : string }
    | Short_name_used_with_dash_dash of Name.t
    | Pos_req_missing of int
    | Named_req_missing of Name.t Nonempty_list.t
    | Named_opt_appeared_multiple_times of (Name.t Nonempty_list.t * int)
    | Named_req_appeared_multiple_times of (Name.t Nonempty_list.t * int)
    | Flag_appeared_multiple_times of (Name.t Nonempty_list.t * int)
    | Conv_failed of
        { locator : [ `Named of Name.t | `Positional of int ] option
        ; message : string
        }
    | Too_many_positional_arguments of { max : int }
    | Invalid_char_in_argument_name of
        { attempted_argument_name : string
        ; invalid_char : char
        }

  exception E of t

  let to_string = function
    | Arg_lacks_param name ->
      sprintf "Named argument %S lacks parameter." (Name.to_string_with_dashes name)
    | Flag_has_param { name; value } ->
      sprintf
        "Flag %s does not take a parameter but was passed: %S"
        (Name.to_string_with_dashes name)
        value
    | No_such_arg name ->
      sprintf "Unknown argument name: %s" (Name.to_string_with_dashes name)
    | Name_would_begin_with_dash string ->
      sprintf
        "%S is not a valid argument specifier as it begins with 3 dashes. Only a single \
         dash or two dashes may be used to denote an argument."
        string
    | Short_name_would_be_dash { entire_short_sequence } ->
      sprintf
        "Encountered dash while parsing sequence of short names \"-%s\". Each character \
         in a sequence of short names is interpreted as a short name, but dashes may not \
         be used as short names."
        entire_short_sequence
    | Short_name_used_with_dash_dash name ->
      sprintf
        "Single-character names must only be specified with a single dash. \"--%s\" is \
         not allowed as it has two dashes but only one character."
        (Name.to_string name)
    | Pos_req_missing i ->
      sprintf "Missing required positional argument at position %d." i
    | Named_req_missing names ->
      sprintf "Missing required named argument: %s" (names_to_string_hum names)
    | Named_opt_appeared_multiple_times (names, n) ->
      sprintf
        "The option %S was passed %d times but may only appear at most once."
        (names_to_string_hum names)
        n
    | Named_req_appeared_multiple_times (names, n) ->
      sprintf
        "The argument %S was passed %d times but must be passed exactly once."
        (names_to_string_hum names)
        n
    | Flag_appeared_multiple_times (names, n) ->
      sprintf
        "The flag %S was passed %d times but must may only appear at most once."
        (names_to_string_hum names)
        n
    | Conv_failed { locator; message } ->
      (match locator with
       | Some (`Named name) ->
         sprintf
           "Failed to parse the argument to %S: %s"
           (Name.to_string_with_dashes name)
           message
       | Some (`Positional i) ->
         sprintf "Failed to parse the argument at position %d: %s" i message
       | None -> sprintf "Failed to parse the argument: %s" message)
    | Too_many_positional_arguments { max } ->
      sprintf
        "Too many positional arguments. At most %d positional arguments may be passed."
        max
    | Invalid_char_in_argument_name { attempted_argument_name; invalid_char } ->
      sprintf
        "Invalid character %C in argument name %S"
        invalid_char
        attempted_argument_name
  ;;

  let exit_code = 124
end

module Spec_error = struct
  type t =
    | Empty_name_list
    | Duplicate_name of Name.t
    | Invalid_name of (string * Name.Invalid.t)
    | Negative_position of int
    | Duplicate_enum_names of string list
    | No_such_enum_value of { valid_names : string list }
    | Gap_in_positional_argument_range of int
    | Name_reserved_for_help of Name.t
    | Positional_argument_collision_with_different_value_names of
        { index : int
        ; value_name1 : string
        ; value_name2 : string
        }
    | Conflicting_requiredness_for_positional_argument of int

  exception E of t

  let to_string = function
    | Empty_name_list -> "Name list is empty"
    | Duplicate_name name ->
      sprintf
        "The name %S is used in multiple arguments."
        (Name.to_string_with_dashes name)
    | Invalid_name (attempted_name, invalid) ->
      let reason =
        match invalid with
        | Name.Invalid.Begins_with_dash -> "it begins with a dash"
        | Empty_name -> "it is the empty string"
        | Invalid_char char -> sprintf "it contains the character %C" char
      in
      sprintf
        "Attempted to use %S as an argument name. %S is not a valid argument name \
         because %s which is not allowed."
        attempted_name
        attempted_name
        reason
    | Negative_position i ->
      sprintf "Attempted to declare positional argument with negative position: %d" i
    | Duplicate_enum_names names ->
      sprintf
        "An enum was declared with duplicate names. The following names were duplicated: \
         %s"
        (String.concat ~sep:" " names)
    | No_such_enum_value { valid_names } ->
      sprintf
        "Attempted to format an enum value as a string but the value does not appear in \
         the enum declaration. Valid names for this enum are: %s"
        (String.concat ~sep:" " valid_names)
    | Gap_in_positional_argument_range i ->
      sprintf
        "Attempted to declare a parser with a gap in its positional arguments. No parser \
         would interpret the argument at position %d but there is a parser for at least \
         one argument at a higher position."
        i
    | Name_reserved_for_help name ->
      sprintf
        "The name %S can't be used as it's reserved for printing help messages."
        (Name.to_string_with_dashes name)
    | Positional_argument_collision_with_different_value_names
        { index; value_name1; value_name2 } ->
      sprintf
        "The positional argument with index %d was defined multiple times with different \
         value names: %S and %S"
        index
        value_name1
        value_name2
    | Conflicting_requiredness_for_positional_argument index ->
      sprintf
        "Multiple positional arguments registered at the same index (%d) with different \
         requiredness"
        index
  ;;
end

let spec_error error =
  Printf.eprintf "%s" (Spec_error.to_string error);
  raise (Spec_error.E error)
;;
