open! Import
module Nonempty_list = Climate_stdlib.Nonempty_list

let sprintf = Printf.sprintf

module Names = struct
  type t = Name.t Nonempty_list.t

  let to_string_hum t =
    Nonempty_list.to_list t
    |> List.map ~f:Name.to_string_with_dashes
    |> String.concat ~sep:","
  ;;
end

module Command_line = struct
  let from_env () = Sys.argv |> Array.to_list |> List.tl
end

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
    | Named_req_missing of Names.t
    | Named_opt_appeared_multiple_times of (Names.t * int)
    | Named_req_appeared_multiple_times of (Names.t * int)
    | Flag_appeared_multiple_times of (Names.t * int)
    | Incomplete_command
    | Conv_failed of
        { locator : [ `Named of Name.t | `Positional of int ]
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
      sprintf "Missing required named argument: %s" (Names.to_string_hum names)
    | Named_opt_appeared_multiple_times (names, n) ->
      sprintf
        "The option %S was passed %d times but may only appear at most once."
        (Names.to_string_hum names)
        n
    | Named_req_appeared_multiple_times (names, n) ->
      sprintf
        "The argument %S was passed %d times but must be passed exactly once."
        (Names.to_string_hum names)
        n
    | Flag_appeared_multiple_times (names, n) ->
      sprintf
        "The flag %S was passed %d times but must may only appear at most once."
        (Names.to_string_hum names)
        n
    | Incomplete_command ->
      "The command is incomplete. Additional subcommands are required to form a command."
    | Conv_failed { locator; message } ->
      let locator_string =
        match locator with
        | `Named name -> sprintf "the argument to %S" (Name.to_string_with_dashes name)
        | `Positional i -> sprintf "the argument at position %d" i
      in
      sprintf "Failed to parse %s: %s" locator_string message
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
    | Duplicate_name of Name.t
    | Invalid_name of (string * Name.Invalid.t)
    | Negative_position of int
    | Duplicate_enum_names of string list
    | No_such_enum_value of { valid_names : string list }
    | Gap_in_positional_argument_range of int

  exception E of t

  let to_string = function
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
  ;;
end

module Implementation_error = struct
  (* Errors in the implementation of a parser. These are exposed so clients
     can implement custom parsers. *)
  type t =
    | No_such_arg of Name.t
    | Not_a_flag of Name.t
    | Not_an_opt of Name.t

  exception E of t
end

let name_of_string_exn string =
  match Name.of_string string with
  | Ok name -> name
  | Error e -> raise Spec_error.(E (Invalid_name (string, e)))
;;

module Spec = struct
  module Named = struct
    type arg_info = { has_arg : bool }
    type t = arg_info Name.Map.t

    let empty = Name.Map.empty

    let add t name ~has_arg =
      if Name.Map.mem name t
      then raise Spec_error.(E (Duplicate_name name))
      else Name.Map.add t ~key:name ~data:{ has_arg }
    ;;

    let add_names t names ~has_arg =
      List.fold_left (Nonempty_list.to_list names) ~init:t ~f:(fun acc name ->
        add acc name ~has_arg)
    ;;

    let flag names = add_names empty names ~has_arg:false
    let opt names = add_names empty names ~has_arg:true

    let merge x y =
      Name.Map.fold y ~init:x ~f:(fun ~key ~data acc -> add acc key ~has_arg:data.has_arg)
    ;;
  end

  module Positional = struct
    (* Keeps track of which indices of positional argument have parsers registered *)
    type t =
      { all_above_inclusive : int option
      ; other : Int.Set.t
      }

    let empty = { all_above_inclusive = None; other = Int.Set.empty }
    let add_index t index = { t with other = Int.Set.add index t.other }

    let add_all_above_inclusive t index =
      match t.all_above_inclusive with
      | Some x when x < index -> t
      | _ -> { t with all_above_inclusive = Some index }
    ;;

    let add_all_below_exclusive t index =
      let other =
        Seq.init index Fun.id |> Seq.fold_left (fun acc i -> Int.Set.add i acc) t.other
      in
      { t with other }
    ;;

    let merge x y =
      let other = Int.Set.union x.other y.other in
      let all_above_inclusive =
        match x.all_above_inclusive, y.all_above_inclusive with
        | None, None -> None
        | Some a, None | None, Some a -> Some a
        | Some x, Some y -> Some (Int.min x y)
      in
      { all_above_inclusive; other }
    ;;

    let index i = add_index empty i
    let all_above_inclusive i = add_all_above_inclusive empty i
    let all_below_exclusive i = add_all_below_exclusive empty i

    (* Check that there are no gaps in the declared positional arguments (E.g.
       if the parser would interpret the argument at position 0 and 2 but not 1
       it's probably an error.) *)
    let validate_no_gaps { all_above_inclusive; other } =
      let set_to_validate =
        match all_above_inclusive with
        | Some i -> Int.Set.add i other
        | None -> other
      in
      match Int.Set.max_elt_opt set_to_validate with
      | None -> Ok ()
      | Some max ->
        let gap =
          Seq.init max Fun.id |> Seq.find (fun i -> not (Int.Set.mem i set_to_validate))
        in
        (match gap with
         | Some i -> Error (Spec_error.Gap_in_positional_argument_range i)
         | None -> Ok ())
    ;;

    let arg_count { all_above_inclusive; other } =
      match all_above_inclusive with
      | Some _ -> `Unlimited
      | None -> `Limited (Int.Set.cardinal other)
    ;;
  end

  type t =
    { named : Named.t
    ; positional : Positional.t
    }

  let merge x y =
    { named = Named.merge x.named y.named
    ; positional = Positional.merge x.positional y.positional
    }
  ;;

  let named named = { named; positional = Positional.empty }
  let positional positional = { named = Named.empty; positional }
  let empty = { named = Named.empty; positional = Positional.empty }
end

module Raw_arg_table = struct
  (* This data structure holds all the raw arguments parsed from the command
     line before any conversions are applied, along with the spec that they
     were parsed under. Extracting the appropriate value from the table and
     converting it to the required type is performed by the arg parsers
     themselves ([_ Arg_parser.t]s). *)
  type t =
    { spec : Spec.t
    ; pos : string list
    ; flag_counts : int Name.Map.t
    ; opts : string list Name.Map.t
    }

  let get_pos_all t = t.pos
  let get_pos t i = List.nth_opt t.pos i

  let get_flag_count t name =
    match Name.Map.find t.spec.named name with
    | None -> raise (Implementation_error.E (No_such_arg name))
    | Some { Spec.Named.has_arg = true } ->
      raise (Implementation_error.E (Not_a_flag name))
    | Some { has_arg = false } ->
      Name.Map.find t.flag_counts name |> Option.value ~default:0
  ;;

  let get_opts t name =
    match Name.Map.find t.spec.named name with
    | None -> raise (Implementation_error.E (No_such_arg name))
    | Some { Spec.Named.has_arg = false } ->
      raise (Implementation_error.E (Not_an_opt name))
    | Some { has_arg = true } -> Name.Map.find t.opts name |> Option.value ~default:[]
  ;;

  let get_flag_count_names t names =
    Nonempty_list.to_list names
    |> List.fold_left ~init:0 ~f:(fun acc name -> acc + get_flag_count t name)
  ;;

  (* Returns a list of [Name.t * string] tuples associating values with the
     corresponding option names from the command line. This is to help with
     error messages since a single option may have multiple names and we want
     to print the specific name used to pass errorneous option values in
     error messages. *)
  let get_opts_names_by_name t names =
    Nonempty_list.to_list names
    |> List.concat_map ~f:(fun name ->
      get_opts t name |> List.map ~f:(fun value -> name, value))
  ;;

  let empty spec = { spec; pos = []; flag_counts = Name.Map.empty; opts = Name.Map.empty }

  let add_opt t ~name ~value =
    { t with
      opts =
        Name.Map.update t.opts ~key:name ~f:(function
          | None -> Some [ value ]
          | Some values -> Some (value :: values))
    }
  ;;

  let add_flag t ~name =
    { t with
      flag_counts =
        Name.Map.update t.flag_counts ~key:name ~f:(function
          | None -> Some 1
          | Some i -> Some (i + 1))
    }
  ;;

  let add_pos t arg =
    let ret = { t with pos = arg :: t.pos } in
    match Spec.Positional.arg_count t.spec.positional with
    | `Unlimited -> Ok ret
    | `Limited count ->
      if List.length ret.pos > count
      then Error (Parse_error.Too_many_positional_arguments { max = count })
      else Ok ret
  ;;

  let reverse_lists t =
    { t with pos = List.rev t.pos; opts = Name.Map.map t.opts ~f:List.rev }
  ;;

  let parse_long_name t term_after_dash_dash remaining_args =
    if String.starts_with term_after_dash_dash ~prefix:"-"
    then Error (Parse_error.Name_would_begin_with_dash term_after_dash_dash)
    else (
      match String.lsplit2 term_after_dash_dash ~on:'=' with
      | Some (term_after_dash_dash, value) ->
        let name = Name.of_string_exn term_after_dash_dash in
        (match Name.kind name with
         | `Short -> Error (Parse_error.Short_name_used_with_dash_dash name)
         | `Long ->
           (match Name.Map.find t.spec.named name with
            | None -> Error (Parse_error.No_such_arg name)
            | Some { Spec.Named.has_arg = false } ->
              Error (Parse_error.Flag_has_param { name; value })
            | Some { has_arg = true } -> Ok (add_opt t ~name ~value, remaining_args)))
      | None ->
        let name = Name.of_string_exn term_after_dash_dash in
        (match Name.kind name with
         | `Short -> Error (Parse_error.Short_name_used_with_dash_dash name)
         | `Long ->
           (match Name.Map.find t.spec.named name with
            | None -> Error (Parse_error.No_such_arg name)
            | Some { Spec.Named.has_arg = false } -> Ok (add_flag t ~name, remaining_args)
            | Some { has_arg = true } ->
              (match remaining_args with
               | [] -> Error (Parse_error.Arg_lacks_param name)
               | x :: xs -> Ok (add_opt t ~name ~value:x, xs)))))
  ;;

  let parse_short_name t name remaining_short_sequence remaining_args =
    match Name.Map.find t.spec.named name with
    | None -> Error (Parse_error.No_such_arg name)
    | Some { Spec.Named.has_arg = false } ->
      Ok (add_flag t ~name, remaining_short_sequence, remaining_args)
    | Some { has_arg = true } ->
      if String.is_empty remaining_short_sequence
      then (
        match remaining_args with
        | [] ->
          (* There are no more terms on the command line and this is the last
             character of the short sequence, yet the current argument requires
             a parameter. *)
          Error (Parse_error.Arg_lacks_param name)
        | x :: xs ->
          (* Treat the next term on the command line as the parameter to the
             current argument. *)
          Ok (add_opt t ~name ~value:x, remaining_short_sequence, xs))
      else
        (* Treat the remainder of the short sequence as the parameter. *)
        Ok (add_opt t ~name ~value:remaining_short_sequence, "", remaining_args)
  ;;

  (* Parse a sequence of short arguments. If one of the arguments takes a
     parameter then the remainder of the string is treated as that parameter.
  *)
  let parse_short_sequence t short_sequence remaining_args =
    let open Result.O in
    let rec loop acc remaining_short_sequence remaining_args =
      match Name.chip_short_name_off_string remaining_short_sequence with
      | Error Name.Invalid.Empty_name -> Ok (acc, remaining_args)
      | Error Begins_with_dash ->
        Error
          (Parse_error.Short_name_would_be_dash { entire_short_sequence = short_sequence })
      | Error (Invalid_char invalid_char) ->
        Error
          (Parse_error.Invalid_char_in_argument_name
             { attempted_argument_name = String.make 1 invalid_char; invalid_char })
      | Ok (name, remaining_short_sequence) ->
        let* acc, remaining_short_sequence, remaining_args =
          parse_short_name acc name remaining_short_sequence remaining_args
        in
        loop acc remaining_short_sequence remaining_args
    in
    loop t short_sequence remaining_args
  ;;

  let parse (spec : Spec.t) args =
    let open Result.O in
    let rec loop (acc : t) = function
      | [] -> Ok acc
      | "--" :: xs ->
        (* all arguments after a "--" argument are treated as positional *)
        Result.List.fold_left xs ~init:acc ~f:add_pos
      | "-" :: xs ->
        (* a "-" on its own is treated as positional *)
        Result.bind (add_pos acc "-") ~f:(fun acc -> loop acc xs)
      | x :: xs ->
        (match String.drop_prefix x ~prefix:"--" with
         | Some name_string ->
           let* acc, xs = parse_long_name acc name_string xs in
           loop acc xs
         | None ->
           (* x doesn't begin with "--" *)
           (match String.drop_prefix x ~prefix:"-" with
            | Some short_sequence ->
              let* acc, xs = parse_short_sequence acc short_sequence xs in
              loop acc xs
            | None -> Result.bind (add_pos acc x) ~f:(fun acc -> loop acc xs)))
    in
    loop (empty spec) args |> Result.map ~f:reverse_lists
  ;;
end

module Arg_parser = struct
  module Conv = struct
    type 'a parse = string -> ('a, [ `Msg of string ]) result
    type 'a print = Format.formatter -> 'a -> unit

    type 'a t =
      { parse : 'a parse
      ; print : 'a print
      }
  end

  type 'a conv = 'a Conv.t =
    { parse : 'a Conv.parse
    ; print : 'a Conv.print
    }

  let sprintf = Printf.sprintf
  let string = { parse = Result.ok; print = Format.pp_print_string }

  let int =
    let parse s =
      match int_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an int)" s))
    in
    { parse; print = Format.pp_print_int }
  ;;

  let float =
    let parse s =
      match float_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an float)" s))
    in
    { parse; print = Format.pp_print_float }
  ;;

  let bool =
    let parse s =
      match bool_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an bool)" s))
    in
    { parse; print = Format.pp_print_bool }
  ;;

  let enum l ~eq =
    let all_names = List.map l ~f:fst in
    let duplicate_names =
      List.fold_left
        all_names
        ~init:(String.Set.empty, [])
        ~f:(fun (set, duplicate_names) name ->
          if String.Set.mem name set
          then set, name :: duplicate_names
          else String.Set.add name set, duplicate_names)
      |> snd
      |> List.rev
    in
    if List.length duplicate_names > 0
    then raise Spec_error.(E (Duplicate_enum_names duplicate_names));
    let parse s =
      let value_opt =
        List.find_map l ~f:(fun (name, value) ->
          if String.equal name s then Some value else None)
      in
      match value_opt with
      | Some value -> Ok value
      | None ->
        let all_names_string = String.concat ~sep:", " all_names in
        let message =
          sprintf "invalid value: %S (valid values are: %s)" s all_names_string
        in
        Error (`Msg message)
    in
    let print ppf v =
      let name =
        List.find_map l ~f:(fun (name, value) -> if eq value v then Some name else None)
      in
      match name with
      | Some name -> Format.pp_print_string ppf name
      | None ->
        raise Spec_error.(E (No_such_enum_value { valid_names = List.map l ~f:fst }))
    in
    { parse; print }
  ;;

  type 'a arg_compute = Raw_arg_table.t -> 'a

  (* A parser for an argument or set of arguments. Typically parsers for each
     argument are combined into a single giant parser that parses all arguments
     to a program either returning some record containing all values or
     returning a unit and having the side effect of running the entire program
     once parsing is complete. A parser is made up of a spec that tells the low
     level parser in [Raw_arg_table] how to interpret terms on the command
     line, and a function [arg_compute] which knows how to retrieve the
     necessary raw values from a [Raw_arg_table.t] and convert them into the
     appropriate type for the parser. *)
  type 'a t =
    { arg_spec : Spec.t
    ; arg_compute : 'a arg_compute
    }

  type 'a nonempty_list = 'a Nonempty_list.t = ( :: ) of ('a * 'a list)

  let map { arg_spec; arg_compute } ~f =
    { arg_spec; arg_compute = (fun name_table -> f (arg_compute name_table)) }
  ;;

  let both x y =
    { arg_spec = Spec.merge x.arg_spec y.arg_spec
    ; arg_compute =
        (fun name_table ->
          let x_value = x.arg_compute name_table in
          let y_value = y.arg_compute name_table in
          x_value, y_value)
    }
  ;;

  let ( >>| ) t f = map t ~f
  let ( let+ ) = ( >>| )
  let ( and+ ) = both

  let names_of_strings =
    Nonempty_list.map ~f:(fun string ->
      match Name.of_string string with
      | Ok name -> name
      | Error invalid -> raise Spec_error.(E (Invalid_name (string, invalid))))
  ;;

  let const x = { arg_spec = Spec.empty; arg_compute = Fun.const x }

  let named_multi names conv =
    let names = names_of_strings names in
    { arg_spec = Spec.named (Spec.Named.opt names)
    ; arg_compute =
        (fun name_table ->
          Raw_arg_table.get_opts_names_by_name name_table names
          |> List.map ~f:(fun (name, value) ->
            match conv.parse value with
            | Ok value -> value
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Named name; message }))))
    }
  ;;

  let named_opt names conv =
    named_multi names conv
    |> map ~f:(function
      | [] -> None
      | [ x ] -> Some x
      | many ->
        raise
          Parse_error.(
            E
              (Named_opt_appeared_multiple_times (names_of_strings names, List.length many))))
  ;;

  let named_req names conv =
    named_multi names conv
    |> map ~f:(function
      | [] -> raise Parse_error.(E (Named_req_missing (names_of_strings names)))
      | [ x ] -> x
      | many ->
        raise
          Parse_error.(
            E
              (Named_req_appeared_multiple_times (names_of_strings names, List.length many))))
  ;;

  let flag_count names =
    let names = names_of_strings names in
    { arg_spec = Spec.named (Spec.Named.flag names)
    ; arg_compute =
        (fun name_table -> Raw_arg_table.get_flag_count_names name_table names)
    }
  ;;

  let flag names =
    flag_count names
    |> map ~f:(function
      | 0 -> false
      | 1 -> true
      | n ->
        raise Parse_error.(E (Flag_appeared_multiple_times (names_of_strings names, n))))
  ;;

  let pos_opt i conv =
    let i =
      match Nonnegative_int.of_int i with
      | Some _ -> i
      | None -> raise Spec_error.(E (Negative_position i))
    in
    { arg_spec = Spec.positional (Spec.Positional.index i)
    ; arg_compute =
        (fun name_table ->
          Raw_arg_table.get_pos name_table i
          |> Option.map ~f:(fun x ->
            match conv.parse x with
            | Ok x -> x
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Positional i; message }))))
    }
  ;;

  let pos_req i conv =
    pos_opt i conv
    |> map ~f:(function
      | Some x -> x
      | None -> raise Parse_error.(E (Pos_req_missing i)))
  ;;

  let pos_left i conv =
    { arg_spec = Spec.positional (Spec.Positional.all_below_exclusive i)
    ; arg_compute =
        (fun name_table ->
          let left, _ = List.split_n (Raw_arg_table.get_pos_all name_table) i in
          List.mapi left ~f:(fun i x ->
            match conv.parse x with
            | Ok x -> x
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Positional i; message }))))
    }
  ;;

  let pos_right i conv =
    { arg_spec = Spec.positional (Spec.Positional.all_above_inclusive i)
    ; arg_compute =
        (fun name_table ->
          let _, right = List.split_n (Raw_arg_table.get_pos_all name_table) i in
          List.mapi right ~f:(fun i x ->
            match conv.parse x with
            | Ok x -> x
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Positional i; message }))))
    }
  ;;

  let pos_all conv = pos_right 0 conv

  let eval t command_line =
    let arg_table =
      match Raw_arg_table.parse t.arg_spec command_line with
      | Ok x -> x
      | Error e -> raise (Parse_error.E e)
    in
    t.arg_compute arg_table
  ;;

  let validate t =
    match Spec.Positional.validate_no_gaps t.arg_spec.positional with
    | Ok () -> ()
    | Error e -> raise (Spec_error.E e)
  ;;
end

module Command = struct
  type 'a t =
    | Singleton of 'a Arg_parser.t
    | Group of
        { children : (Name.t * 'a t) list
        ; default_term : 'a Arg_parser.t option
        }

  let singleton term =
    Arg_parser.validate term;
    Singleton term
  ;;

  let group ?default_term children =
    (match default_term with
     | Some default_term -> Arg_parser.validate default_term
     | None -> ());
    let children =
      List.map children ~f:(fun (name_string, command) ->
        name_of_string_exn name_string, command)
    in
    Group { children; default_term }
  ;;

  type 'a traverse =
    { term : 'a Arg_parser.t
    ; command_line : string list
    }

  let rec traverse t command_line =
    match t, command_line with
    | Singleton term, command_line -> Ok { term; command_line }
    | Group { children; default_term }, x :: xs ->
      let subcommand =
        List.find_map children ~f:(fun (name, command) ->
          if String.equal (Name.to_string name) x then Some command else None)
      in
      (match subcommand with
       | Some subcommand -> traverse subcommand xs
       | None ->
         (match default_term with
          | Some term -> Ok { term; command_line = x :: xs }
          | None -> Error Parse_error.Incomplete_command))
    | Group { children = _; default_term }, [] ->
      (match default_term with
       | Some term -> Ok { term; command_line = [] }
       | None -> Error Parse_error.Incomplete_command)
  ;;

  let eval t command_line =
    let { term; command_line } =
      match traverse t command_line with
      | Ok x -> x
      | Error e -> raise (Parse_error.E e)
    in
    Arg_parser.eval term command_line
  ;;

  let run t =
    try Command_line.from_env () |> eval t with
    | Parse_error.E e ->
      Printf.eprintf "%s" (Parse_error.to_string e);
      exit Parse_error.exit_code
  ;;
end

module For_test = struct
  include For_test
  module Parse_error = Parse_error
end
