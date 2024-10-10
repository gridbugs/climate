open! Import

module Parse_error : sig
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

  val to_string : t -> string
  val exit_code : int
end

module Spec_error : sig
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

  val to_string : t -> string
end

val spec_error : Spec_error.t -> _
