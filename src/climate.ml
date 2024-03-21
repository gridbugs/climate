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
  type t =
    { program : string
    ; args : string list
    }

  let from_env () =
    match Sys.argv |> Array.to_list with
    | program :: args -> { program; args }
    | [] -> failwith "unable to read command-line arguments from environment"
  ;;
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
    | Name_reserved_for_help of Name.t
    | Positional_argument_collision_with_different_value_names of
        { index : int
        ; value_name1 : string
        ; value_name2 : string
        }
    | Conflicting_requiredness_for_positional_argument of int

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

let help_names : Name.t Nonempty_list.t =
  [ Name.of_string_exn "help"; Name.of_string_exn "h" ]
;;

module Spec = struct
  module Named = struct
    module Info = struct
      type t =
        { names : Name.t Nonempty_list.t
        ; has_param : [ `No | `Yes_with_value_name of string ]
        ; default_string :
            string option (* default value to display in documentation (if any) *)
        ; required : bool (* determines if argument is shown in usage string *)
        ; desc : string option
        }

      let has_param t =
        match t.has_param with
        | `No -> false
        | `Yes_with_value_name _ -> true
      ;;

      let flag names ~desc =
        { names; has_param = `No; default_string = None; required = false; desc }
      ;;

      let long_name { names; _ } =
        List.find_opt (Nonempty_list.to_list names) ~f:Name.is_long
      ;;

      let choose_name_long_if_possible t =
        match long_name t with
        | Some name -> name
        | None -> Nonempty_list.hd t.names
      ;;
    end

    type t = { infos : Info.t list }

    let empty = { infos = [] }

    let get_info_by_name { infos } name =
      List.find_opt infos ~f:(fun (info : Info.t) ->
        List.exists (Nonempty_list.to_list info.names) ~f:(Name.equal name))
    ;;

    let contains_name { infos } name =
      List.exists infos ~f:(fun (info : Info.t) ->
        List.exists (Nonempty_list.to_list info.names) ~f:(Name.equal name))
    ;;

    let add t (info : Info.t) =
      List.iter (Nonempty_list.to_list info.names) ~f:(fun name ->
        if contains_name t name then raise Spec_error.(E (Duplicate_name name)));
      { infos = info :: t.infos }
    ;;

    let merge x y = List.fold_left y.infos ~init:x ~f:add

    let validate_no_reserved_help_names t =
      match
        List.find_map (Nonempty_list.to_list help_names) ~f:(fun name ->
          if contains_name t name then Some name else None)
      with
      | None -> Ok ()
      | Some help_name -> Error (Spec_error.Name_reserved_for_help help_name)
    ;;

    let all_required { infos } =
      List.filter infos ~f:(fun { Info.required; _ } -> required)
    ;;

    let all_optional { infos } =
      List.filter infos ~f:(fun { Info.required; _ } -> not required)
    ;;

    let to_autocompletion_arg { infos } =
      List.concat_map infos ~f:(fun (info : Info.t) ->
        let has_param =
          match info.has_param with
          | `No -> false
          | `Yes_with_value_name _ -> true
        in
        List.filter_map (Nonempty_list.to_list info.names) ~f:(fun name ->
          if Name.is_long name
          then Some { Autocompletion.Arg.name = Name.to_string name; has_param }
          else None))
    ;;
  end

  module Positional = struct
    type all_above_inclusive =
      { index : int
      ; value_name : string
      }

    type single_arg =
      { required : bool
      ; value_name : string
      }

    (* Keeps track of which indices of positional argument have parsers registered *)
    type t =
      { all_above_inclusive : all_above_inclusive option
      ; other_value_names_by_index : single_arg Int.Map.t
      }

    let empty = { all_above_inclusive = None; other_value_names_by_index = Int.Map.empty }

    let check_value_names index value_name1 value_name2 =
      if not (String.equal value_name1 value_name2)
      then
        raise
          Spec_error.(
            E
              (Positional_argument_collision_with_different_value_names
                 { index; value_name1; value_name2 }))
    ;;

    let trim_map t =
      match t.all_above_inclusive with
      | None -> t
      | Some all_above_inclusive ->
        let other_value_names_by_index =
          Int.Map.filter
            t.other_value_names_by_index
            ~f:(fun index { value_name; required } ->
              if index >= all_above_inclusive.index
              then (
                check_value_names index value_name all_above_inclusive.value_name;
                if required
                then
                  raise
                    Spec_error.(
                      E (Conflicting_requiredness_for_positional_argument index));
                false)
              else true)
        in
        { t with other_value_names_by_index }
    ;;

    let add_index t index ~value_name ~required =
      let other_value_names_by_index =
        Int.Map.update t.other_value_names_by_index ~key:index ~f:(function
          | None -> Some { value_name; required }
          | Some x ->
            check_value_names index x.value_name value_name;
            if x.required <> required
            then
              raise
                Spec_error.(E (Conflicting_requiredness_for_positional_argument index));
            Some x)
      in
      trim_map { t with other_value_names_by_index }
    ;;

    let add_all_above_inclusive t index ~value_name =
      match t.all_above_inclusive with
      | Some x when x.index < index ->
        check_value_names index x.value_name value_name;
        t
      | _ -> trim_map { t with all_above_inclusive = Some { index; value_name } }
    ;;

    let add_all_below_exclusive t index ~value_name ~required =
      Seq.init index Fun.id
      |> Seq.fold_left (add_index ~value_name ~required) t
      |> trim_map
    ;;

    let merge x y =
      let all_above_inclusive =
        match x.all_above_inclusive, y.all_above_inclusive with
        | None, None -> None
        | Some a, None | None, Some a -> Some a
        | Some x, Some y ->
          check_value_names (Int.max x.index y.index) x.value_name y.value_name;
          let index = Int.min x.index y.index in
          Some { index; value_name = x.value_name }
      in
      let other_value_names_by_index =
        Int.Map.merge
          x.other_value_names_by_index
          y.other_value_names_by_index
          ~f:(fun index x y ->
            match x, y with
            | None, None -> None
            | Some value_name, None | None, Some value_name -> Some value_name
            | Some x, Some y ->
              check_value_names index x.value_name y.value_name;
              if x.required <> y.required
              then
                raise
                  Spec_error.(E (Conflicting_requiredness_for_positional_argument index));
              Some x)
      in
      trim_map { all_above_inclusive; other_value_names_by_index }
    ;;

    let index i = add_index empty i
    let all_above_inclusive i = add_all_above_inclusive empty i
    let all_below_exclusive i = add_all_below_exclusive empty i

    (* Check that there are no gaps in the declared positional arguments (E.g.
       if the parser would interpret the argument at position 0 and 2 but not 1
       it's probably an error.) *)
    let validate_no_gaps { all_above_inclusive; other_value_names_by_index } =
      let other_indices =
        Int.Map.to_seq other_value_names_by_index |> Seq.map fst |> Int.Set.of_seq
      in
      let set_to_validate =
        match all_above_inclusive with
        | Some { index; _ } -> Int.Set.add index other_indices
        | None -> other_indices
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

    let arg_count { all_above_inclusive; other_value_names_by_index } =
      match all_above_inclusive with
      | Some _ -> `Unlimited
      | None -> `Limited (Int.Map.cardinal other_value_names_by_index)
    ;;

    let all_required_value_names { other_value_names_by_index; _ } =
      Int.Map.to_seq other_value_names_by_index
      |> Seq.filter_map (fun (_, { required; value_name }) ->
        if required then Some value_name else None)
      |> List.of_seq
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

  let empty = { named = Named.empty; positional = Positional.empty }
  let positional positional = { named = Named.empty; positional }

  let named info =
    let named = Named.add Named.empty info in
    { named; positional = Positional.empty }
  ;;

  let flag names ~desc = named (Named.Info.flag names ~desc)

  let usage ppf { named; positional } =
    let named_optional = Named.all_optional named in
    if not (List.is_empty named_optional) then Format.pp_print_string ppf " [OPTIONS]";
    let named_required = Named.all_required named in
    List.iter named_required ~f:(fun (info : Named.Info.t) ->
      match info.has_param with
      | `No ->
        (* there should be no required arguments with no parameters *)
        ()
      | `Yes_with_value_name value_name ->
        let name = Named.Info.choose_name_long_if_possible info in
        if Name.is_long name
        then Format.fprintf ppf " %s=<%s>" (Name.to_string_with_dashes name) value_name
        else Format.fprintf ppf " %s<%s>" (Name.to_string_with_dashes name) value_name);
    Positional.all_required_value_names positional
    |> List.iter ~f:(fun value_name -> Format.fprintf ppf " <%s>" value_name);
    match positional.all_above_inclusive with
    | Some { value_name; _ } -> Format.fprintf ppf "[%s]..." value_name
    | None -> ()
  ;;

  let named_help ppf { named; _ } =
    if not (List.is_empty named.infos) then Format.pp_print_string ppf "Options:";
    Format.pp_print_newline ppf ();
    List.iter named.infos ~f:(fun (info : Named.Info.t) ->
      Format.pp_print_string ppf " ";
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
        (fun ppf name -> Format.pp_print_string ppf (Name.to_string_with_dashes name))
        ppf
        (Nonempty_list.to_list info.names);
      (match info.has_param with
       | `No -> ()
       | `Yes_with_value_name value_name ->
         Format.pp_print_string ppf " ";
         Format.fprintf ppf "<%s>" value_name);
      (match info.desc with
       | None -> ()
       | Some desc -> Format.fprintf ppf "   %s" desc);
      Format.pp_print_newline ppf ())
  ;;
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
    match Spec.Named.get_info_by_name t.spec.named name with
    | None -> raise (Implementation_error.E (No_such_arg name))
    | Some info ->
      if Spec.Named.Info.has_param info
      then raise (Implementation_error.E (Not_a_flag name))
      else Name.Map.find t.flag_counts name |> Option.value ~default:0
  ;;

  let get_opts t name =
    match Spec.Named.get_info_by_name t.spec.named name with
    | None -> raise (Implementation_error.E (No_such_arg name))
    | Some info ->
      if Spec.Named.Info.has_param info
      then Name.Map.find t.opts name |> Option.value ~default:[]
      else raise (Implementation_error.E (Not_an_opt name))
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
           (match Spec.Named.get_info_by_name t.spec.named name with
            | None -> Error (Parse_error.No_such_arg name)
            | Some info ->
              if Spec.Named.Info.has_param info
              then Ok (add_opt t ~name ~value, remaining_args)
              else Error (Parse_error.Flag_has_param { name; value })))
      | None ->
        let name = Name.of_string_exn term_after_dash_dash in
        (match Name.kind name with
         | `Short -> Error (Parse_error.Short_name_used_with_dash_dash name)
         | `Long ->
           (match Spec.Named.get_info_by_name t.spec.named name with
            | None -> Error (Parse_error.No_such_arg name)
            | Some info ->
              if Spec.Named.Info.has_param info
              then (
                match remaining_args with
                | [] -> Error (Parse_error.Arg_lacks_param name)
                | x :: xs -> Ok (add_opt t ~name ~value:x, xs))
              else Ok (add_flag t ~name, remaining_args))))
  ;;

  let parse_short_name t name remaining_short_sequence remaining_args =
    match Spec.Named.get_info_by_name t.spec.named name with
    | None -> Error (Parse_error.No_such_arg name)
    | Some info ->
      if Spec.Named.Info.has_param info
      then
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
      else Ok (add_flag t ~name, remaining_short_sequence, remaining_args)
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
  type 'a parse = string -> ('a, [ `Msg of string ]) result
  type 'a print = Format.formatter -> 'a -> unit

  type 'a conv =
    { parse : 'a parse
    ; print : 'a print
    ; default_value_name : string
    }

  let conv_value_to_string conv value =
    conv.print Format.str_formatter value;
    Format.flush_str_formatter ()
  ;;

  let sprintf = Printf.sprintf

  let string =
    { parse = Result.ok; print = Format.pp_print_string; default_value_name = "STRING" }
  ;;

  let int =
    let parse s =
      match int_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an int)" s))
    in
    { parse; print = Format.pp_print_int; default_value_name = "INT" }
  ;;

  let float =
    let parse s =
      match float_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an float)" s))
    in
    { parse; print = Format.pp_print_float; default_value_name = "FLOAT" }
  ;;

  let bool =
    let parse s =
      match bool_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an bool)" s))
    in
    { parse; print = Format.pp_print_bool; default_value_name = "BOOL" }
  ;;

  let enum l ~eq ~default_value_name =
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
    { parse; print; default_value_name }
  ;;

  module Context = struct
    type t =
      { raw_arg_table : Raw_arg_table.t
      ; subcommand : string list
      }
  end

  type 'a arg_compute = Context.t -> 'a

  (* A parser for an argument or set of arguments. Typically parsers for each
     argument are combined into a single giant parser that parses all arguments
     to a program either returning some record containing all values or
     returning a unit and having the side effect of running the entire program
     once parsing is complete. A parser is made up of a spec that tells the low
     level parser in [Raw_arg_table] how to interpret terms on the command
     line, and a function [arg_compute] which knows how to retrieve the
     necessary raw values from a [Context.t] and convert them into the
     appropriate type for the parser. *)
  type 'a t =
    { arg_spec : Spec.t
    ; arg_compute : 'a arg_compute
    }

  type 'a nonempty_list = 'a Nonempty_list.t = ( :: ) of ('a * 'a list)

  let map { arg_spec; arg_compute } ~f =
    { arg_spec; arg_compute = (fun context -> f (arg_compute context)) }
  ;;

  let both x y =
    { arg_spec = Spec.merge x.arg_spec y.arg_spec
    ; arg_compute =
        (fun context ->
          let x_value = x.arg_compute context in
          let y_value = y.arg_compute context in
          x_value, y_value)
    }
  ;;

  let ( >>| ) t f = map t ~f
  let ( let+ ) = ( >>| )
  let ( and+ ) = both
  let names_of_strings = Nonempty_list.map ~f:name_of_string_exn
  let const x = { arg_spec = Spec.empty; arg_compute = Fun.const x }
  let unit = const ()

  let named_multi_gen info conv =
    { arg_spec = Spec.named info
    ; arg_compute =
        (fun context ->
          Raw_arg_table.get_opts_names_by_name context.raw_arg_table info.names
          |> List.map ~f:(fun (name, value) ->
            match conv.parse value with
            | Ok value -> value
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Named name; message }))))
    }
  ;;

  let named_opt_gen (info : Spec.Named.Info.t) conv =
    named_multi_gen info conv
    |> map ~f:(function
      | [] -> None
      | [ x ] -> Some x
      | many ->
        raise
          Parse_error.(
            E (Named_opt_appeared_multiple_times (info.names, List.length many))))
  ;;

  let named_multi ?desc ?value_name names conv =
    named_multi_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = false
      ; desc
      }
      conv
  ;;

  let named_opt ?desc ?value_name names conv =
    named_opt_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = false
      ; desc
      }
      conv
  ;;

  let named_with_default ?desc ?value_name names conv ~default =
    named_opt_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = Some (conv_value_to_string conv default)
      ; required = false
      ; desc
      }
      conv
    >>| Option.value ~default
  ;;

  let named_req ?desc ?value_name names conv =
    named_multi_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = true
      ; desc
      }
      conv
    |> map ~f:(function
      | [] -> raise Parse_error.(E (Named_req_missing (names_of_strings names)))
      | [ x ] -> x
      | many ->
        raise
          Parse_error.(
            E
              (Named_req_appeared_multiple_times (names_of_strings names, List.length many))))
  ;;

  let flag_count ?desc names =
    let names = names_of_strings names in
    { arg_spec = Spec.flag names ~desc
    ; arg_compute =
        (fun context -> Raw_arg_table.get_flag_count_names context.raw_arg_table names)
    }
  ;;

  let flag ?desc names =
    flag_count ?desc names
    |> map ~f:(function
      | 0 -> false
      | 1 -> true
      | n ->
        raise Parse_error.(E (Flag_appeared_multiple_times (names_of_strings names, n))))
  ;;

  let pos_single_gen i conv ~value_name ~required =
    let i =
      match Nonnegative_int.of_int i with
      | Some _ -> i
      | None -> raise Spec_error.(E (Negative_position i))
    in
    { arg_spec =
        Spec.positional
          (Spec.Positional.index
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~required)
    ; arg_compute =
        (fun context ->
          Raw_arg_table.get_pos context.raw_arg_table i
          |> Option.map ~f:(fun x ->
            match conv.parse x with
            | Ok x -> x
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Positional i; message }))))
    }
  ;;

  let pos_opt ?value_name i conv = pos_single_gen i conv ~value_name ~required:false

  let pos_req ?value_name i conv =
    pos_single_gen i conv ~value_name ~required:true
    |> map ~f:(function
      | Some x -> x
      | None -> raise Parse_error.(E (Pos_req_missing i)))
  ;;

  let pos_left_gen i conv ~value_name ~required =
    { arg_spec =
        Spec.positional
          (Spec.Positional.all_below_exclusive
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~required)
    ; arg_compute =
        (fun context ->
          let left, _ =
            List.split_n (Raw_arg_table.get_pos_all context.raw_arg_table) i
          in
          List.mapi left ~f:(fun i x ->
            match conv.parse x with
            | Ok x -> x
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Positional i; message }))))
    }
  ;;

  let pos_left ?value_name i conv = pos_left_gen i conv ~value_name ~required:false

  let pos_right ?value_name i conv =
    { arg_spec =
        Spec.positional
          (Spec.Positional.all_above_inclusive
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name))
    ; arg_compute =
        (fun context ->
          let _, right =
            List.split_n (Raw_arg_table.get_pos_all context.raw_arg_table) i
          in
          List.mapi right ~f:(fun i x ->
            match conv.parse x with
            | Ok x -> x
            | Error (`Msg message) ->
              raise Parse_error.(E (Conv_failed { locator = `Positional i; message }))))
    }
  ;;

  let pos_all ?value_name conv = pos_right ?value_name 0 conv

  let eval t ~args ~subcommand =
    let raw_arg_table =
      match Raw_arg_table.parse t.arg_spec args with
      | Ok x -> x
      | Error e -> raise (Parse_error.E e)
    in
    let context = { Context.raw_arg_table; subcommand } in
    t.arg_compute context
  ;;

  let validate t =
    (match Spec.Positional.validate_no_gaps t.arg_spec.positional with
     | Ok () -> ()
     | Error e -> raise (Spec_error.E e));
    match Spec.Named.validate_no_reserved_help_names t.arg_spec.named with
    | Ok () -> ()
    | Error e -> raise (Spec_error.E e)
  ;;

  let pp_help ppf arg_spec ~subcommand =
    Format.pp_print_string ppf "Usage:";
    List.iter subcommand ~f:(fun part -> Format.fprintf ppf " %s" part);
    Spec.usage ppf arg_spec;
    Format.pp_print_newline ppf ();
    Format.pp_print_newline ppf ();
    Spec.named_help ppf arg_spec
  ;;

  let add_help { arg_spec; arg_compute } =
    let help_spec = Spec.flag help_names ~desc:(Some "Print help") in
    let arg_spec = Spec.merge arg_spec help_spec in
    { arg_spec
    ; arg_compute =
        (fun context ->
          if Raw_arg_table.get_flag_count_names context.raw_arg_table help_names > 0
          then (
            pp_help Format.std_formatter arg_spec ~subcommand:context.subcommand;
            exit 0)
          else arg_compute context)
    }
  ;;

  let finalize t =
    validate t;
    add_help t
  ;;

  let to_autocompletion_arg { arg_spec; _ } =
    Spec.Named.to_autocompletion_arg arg_spec.named
  ;;
end

module Autocompletion_args = struct
  type t = { program_name : string }

  let arg_parser =
    let open Arg_parser in
    let+ program_name =
      named_opt
        ~desc:
          "Name to register this autocompletion script with in the shell. Should be the \
           name of this program's executable. Will default to argv[0]."
        ~value_name:"PROGRAM"
        [ "program-name" ]
        string
    in
    let program_name =
      match program_name with
      | Some program_name -> program_name
      | None -> Sys.argv.(0)
    in
    { program_name }
  ;;
end

module Command = struct
  type internal = Print_autocompletion_script_bash

  module Info = struct
    type t =
      { name : Name.t
      ; hidden : bool
      }
  end

  type 'a t =
    | Singleton of 'a Arg_parser.t
    | Group of
        { children : (Info.t * 'a t) list
        ; default_arg_parser : 'a Arg_parser.t option
        }
    | Internal of internal

  let rec to_autocompletion_spec = function
    | Singleton arg_parser ->
      { Autocompletion.Spec.args = Arg_parser.to_autocompletion_arg arg_parser
      ; subcommands = []
      }
    | Group { children; default_arg_parser } ->
      let args =
        match default_arg_parser with
        | None -> []
        | Some default_arg_parser -> Arg_parser.to_autocompletion_arg default_arg_parser
      in
      { Autocompletion.Spec.args
      ; subcommands =
          List.filter_map children ~f:(fun ((info : Info.t), t) ->
            if info.hidden
            then None
            else
              Some
                { Autocompletion.Spec.name = Name.to_string info.name
                ; spec = to_autocompletion_spec t
                })
      }
    | Internal _ -> Autocompletion.Spec.empty
  ;;

  let autocompletion_script_bash t ~program_name =
    to_autocompletion_spec t |> Autocompletion.generate_bash ~program_name
  ;;

  let singleton term = Singleton (Arg_parser.finalize term)

  type 'a group_arg =
    | Subcommand of string * 'a t
    | Hidden of string * 'a t

  let group ?default_arg_parser children =
    let default_arg_parser = Option.map default_arg_parser ~f:Arg_parser.finalize in
    let children =
      List.map children ~f:(function
        | Subcommand (name_string, subcommand) ->
          { Info.name = name_of_string_exn name_string; hidden = false }, subcommand
        | Hidden (name_string, subcommand) ->
          { Info.name = name_of_string_exn name_string; hidden = true }, subcommand)
    in
    Group { children; default_arg_parser }
  ;;

  let print_autocompletion_script_bash = Internal Print_autocompletion_script_bash

  type 'a traverse =
    { operation : [ `Arg_parser of 'a Arg_parser.t | `Internal of internal ]
    ; args : string list
    ; subcommand : string list
    }

  let rec traverse t args subcommand_acc =
    match t, args with
    | Singleton arg_parser, args ->
      Ok
        { operation = `Arg_parser arg_parser; args; subcommand = List.rev subcommand_acc }
    | Group { children; default_arg_parser }, x :: xs ->
      let subcommand =
        List.find_map children ~f:(fun (({ name; _ } : Info.t), command) ->
          if String.equal (Name.to_string name) x then Some command else None)
      in
      (match subcommand with
       | Some subcommand -> traverse subcommand xs (x :: subcommand_acc)
       | None ->
         (match default_arg_parser with
          | Some arg_parser ->
            Ok
              { operation = `Arg_parser arg_parser
              ; args = x :: xs
              ; subcommand = List.rev subcommand_acc
              }
          | None -> Error Parse_error.Incomplete_command))
    | Group { children = _; default_arg_parser }, [] ->
      (match default_arg_parser with
       | Some arg_parser ->
         Ok
           { operation = `Arg_parser arg_parser
           ; args = []
           ; subcommand = List.rev subcommand_acc
           }
       | None -> Error Parse_error.Incomplete_command)
    | Internal internal, args ->
      Ok { operation = `Internal internal; args; subcommand = List.rev subcommand_acc }
  ;;

  let eval t (command_line : Command_line.t) =
    let { operation; args; subcommand } =
      match traverse t command_line.args [ command_line.program ] with
      | Ok x -> x
      | Error e -> raise (Parse_error.E e)
    in
    match operation with
    | `Arg_parser arg_parser -> Arg_parser.eval arg_parser ~args ~subcommand
    | `Internal Print_autocompletion_script_bash ->
      let arg_parser = Arg_parser.add_help Autocompletion_args.arg_parser in
      let { Autocompletion_args.program_name } =
        Arg_parser.eval arg_parser ~args ~subcommand
      in
      print_endline (autocompletion_script_bash t ~program_name);
      exit 0
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
