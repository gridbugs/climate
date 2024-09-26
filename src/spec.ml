open! Import
module Spec_error = Error.Spec_error

type untyped_completion_function = Command_line.Rich.t -> string list
type untyped_completion_hint = untyped_completion_function Completion_spec.Hint.t

module Named = struct
  module Info = struct
    type t =
      { names : Name.t Nonempty_list.t
      ; has_param : [ `No | `Yes_with_value_name of string ]
      ; default_string :
          string option (* default value to display in documentation (if any) *)
      ; required : bool (* determines if argument is shown in usage string *)
      ; desc : string option
      ; completion : untyped_completion_hint option
      ; hidden : bool
      }

    let has_param t =
      match t.has_param with
      | `No -> false
      | `Yes_with_value_name _ -> true
    ;;

    let flag names ~desc ~hidden =
      { names
      ; has_param = `No
      ; default_string = None
      ; required = false
      ; desc
      ; completion = None
      ; hidden
      }
    ;;

    let long_name { names; _ } =
      List.find_opt (Nonempty_list.to_list names) ~f:Name.is_long
    ;;

    let choose_name_long_if_possible t =
      match long_name t with
      | Some name -> name
      | None -> Nonempty_list.hd t.names
    ;;

    let to_completion_named_args t =
      Nonempty_list.to_list t.names
      |> List.map ~f:(fun name ->
        { Completion_spec.Named_arg.name; has_param = has_param t; hint = t.completion })
    ;;
  end

  type t = { infos : Info.t list }

  let empty = { infos = [] }
  let is_empty { infos } = List.is_empty infos

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
      if contains_name t name then Error.spec_error (Duplicate_name name));
    { infos = info :: t.infos }
  ;;

  let merge x y = List.fold_left y.infos ~init:x ~f:add

  (* Checks that none of the names in the spec are also the names of
     built-in arguments (such as "--help") *)
  let validate_no_built_in_names t =
    match
      List.find_map (Nonempty_list.to_list Built_in.help_names) ~f:(fun name ->
        if contains_name t name then Some name else None)
    with
    | None -> Ok ()
    | Some help_name -> Error (Spec_error.Name_reserved_for_help help_name)
  ;;

  let all_required { infos } = List.filter infos ~f:(fun { Info.required; _ } -> required)

  let all_optional { infos } =
    List.filter infos ~f:(fun { Info.required; _ } -> not required)
  ;;

  let to_completion_named_args { infos } =
    List.concat_map infos ~f:Info.to_completion_named_args
  ;;
end

module Positional = struct
  type all_above_inclusive =
    { index : int
    ; value_name : string
    ; completion : untyped_completion_hint option
    }

  type single_arg =
    { required : bool
    ; value_name : string
    ; completion : untyped_completion_hint option
    }

  (* Keeps track of which indices of positional argument have parsers registered *)
  type t =
    { all_above_inclusive : all_above_inclusive option
    ; other_value_names_by_index : single_arg Int.Map.t
    }

  let empty = { all_above_inclusive = None; other_value_names_by_index = Int.Map.empty }

  let is_empty { all_above_inclusive; other_value_names_by_index } =
    Option.is_none all_above_inclusive && Int.Map.is_empty other_value_names_by_index
  ;;

  let check_value_names index value_name1 value_name2 =
    if not (String.equal value_name1 value_name2)
    then
      Error.spec_error
        (Positional_argument_collision_with_different_value_names
           { index; value_name1; value_name2 })
  ;;

  let trim_map t =
    match t.all_above_inclusive with
    | None -> t
    | Some all_above_inclusive ->
      let other_value_names_by_index =
        Int.Map.filter
          t.other_value_names_by_index
          ~f:(fun index { value_name; required; _ } ->
            if index >= all_above_inclusive.index
            then (
              check_value_names index value_name all_above_inclusive.value_name;
              if required
              then
                Error.spec_error (Conflicting_requiredness_for_positional_argument index);
              false)
            else true)
      in
      { t with other_value_names_by_index }
  ;;

  let add_index t index ~value_name ~required ~completion =
    let other_value_names_by_index =
      Int.Map.update t.other_value_names_by_index ~key:index ~f:(function
        | None -> Some { value_name; required; completion }
        | Some x ->
          check_value_names index x.value_name value_name;
          if x.required <> required
          then Error.spec_error (Conflicting_requiredness_for_positional_argument index);
          Some x)
    in
    trim_map { t with other_value_names_by_index }
  ;;

  let add_all_above_inclusive t index ~value_name ~completion =
    match t.all_above_inclusive with
    | Some x when x.index < index ->
      check_value_names index x.value_name value_name;
      t
    | _ ->
      trim_map { t with all_above_inclusive = Some { index; value_name; completion } }
  ;;

  let add_all_below_exclusive t index ~value_name ~required ~completion =
    Seq.init index Fun.id
    |> Seq.fold_left (add_index ~value_name ~required ~completion) t
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
        Some { index; value_name = x.value_name; completion = x.completion }
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
            then Error.spec_error (Conflicting_requiredness_for_positional_argument index);
            Some x)
    in
    trim_map { all_above_inclusive; other_value_names_by_index }
  ;;

  let single_at_index i = add_index empty i
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

  let to_completions ({ all_above_inclusive; other_value_names_by_index } as t) =
    if Result.is_error (validate_no_gaps t)
    then raise (Invalid_argument "positional argument spec has gaps");
    let finite_args =
      Int.Map.bindings other_value_names_by_index
      |> List.map ~f:(fun (_, { completion; _ }) -> completion)
    in
    let repeated_arg =
      Option.map all_above_inclusive ~f:(fun { completion; _ } ->
        match completion with
        | None -> `No_hint
        | Some hint -> `Hint hint)
    in
    { Completion_spec.Positional_args_hints.finite_args; repeated_arg }
  ;;

  let arg_count { all_above_inclusive; other_value_names_by_index } =
    match all_above_inclusive with
    | Some _ -> `Unlimited
    | None -> `Limited (Int.Map.cardinal other_value_names_by_index)
  ;;

  let all_required_value_names { other_value_names_by_index; _ } =
    Int.Map.to_seq other_value_names_by_index
    |> Seq.filter_map (fun (_, { required; value_name; _ }) ->
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

let is_empty { named; positional } =
  Named.is_empty named && Positional.is_empty positional
;;

let create_positional positional = { named = Named.empty; positional }

let create_named info =
  let named = Named.add Named.empty info in
  { named; positional = Positional.empty }
;;

let create_flag names ~desc ~hidden = create_named (Named.Info.flag names ~desc ~hidden)

let usage ppf { named; positional } =
  let named_optional = Named.all_optional named in
  if not (List.is_empty named_optional) then Format.pp_print_string ppf " [OPTIONS]";
  let named_required = Named.all_required named in
  List.iter named_required ~f:(fun (info : Named.Info.t) ->
    if not info.hidden
    then (
      match info.has_param with
      | `No ->
        (* there should be no required arguments with no parameters *)
        ()
      | `Yes_with_value_name value_name ->
        let name = Named.Info.choose_name_long_if_possible info in
        if Name.is_long name
        then Format.fprintf ppf " %s=<%s>" (Name.to_string_with_dashes name) value_name
        else Format.fprintf ppf " %s<%s>" (Name.to_string_with_dashes name) value_name));
  Positional.all_required_value_names positional
  |> List.iter ~f:(fun value_name -> Format.fprintf ppf " <%s>" value_name);
  match positional.all_above_inclusive with
  | Some { value_name; _ } -> Format.fprintf ppf "[%s]..." value_name
  | None -> ()
;;

let named_help ppf { named; _ } =
  if not (List.is_empty named.infos) then Format.pp_print_string ppf "Options:";
  Format.pp_print_newline ppf ();
  List.iter (List.rev named.infos) ~f:(fun (info : Named.Info.t) ->
    if not info.hidden
    then (
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
      Format.pp_print_newline ppf ()))
;;

let to_completion_parser_spec { named; positional } =
  let named_args = Named.to_completion_named_args named in
  let positional_args_hints = Positional.to_completions positional in
  { Completion_spec.Parser_spec.named_args; positional_args_hints }
;;

let validate { named; positional } =
  (match Positional.validate_no_gaps positional with
   | Ok () -> ()
   | Error e -> Error.spec_error e);
  match Named.validate_no_built_in_names named with
  | Ok () -> ()
  | Error e -> Error.spec_error e
;;
