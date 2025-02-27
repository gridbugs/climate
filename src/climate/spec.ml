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
      ; doc : string option
      ; completion : untyped_completion_hint option
      ; hidden : bool
      ; repeated : bool
      }

    let has_param t =
      match t.has_param with
      | `No -> false
      | `Yes_with_value_name _ -> true
    ;;

    let flag names ~doc ~hidden ~repeated =
      { names
      ; has_param = `No
      ; default_string = None
      ; required = false
      ; doc
      ; completion = None
      ; hidden
      ; repeated
      }
    ;;

    let to_completion_named_arg t =
      { Completion_spec.Named_arg.names = t.names
      ; has_param = has_param t
      ; hint = t.completion
      }
    ;;

    let command_doc_spec t =
      if t.hidden
      then None
      else (
        let value =
          match t.has_param with
          | `No -> None
          | `Yes_with_value_name name ->
            Some { Command_doc_spec.Value.name; required = true }
        in
        Some
          { Command_doc_spec.Named_arg.names = t.names
          ; value
          ; repeated = t.repeated
          ; default_string = t.default_string
          ; doc = t.doc
          })
    ;;
  end

  type t = { infos : Info.t list }

  let empty = { infos = [] }
  let is_empty { infos } = List.is_empty infos

  let command_doc_spec { infos } =
    List.rev infos |> List.filter_map ~f:Info.command_doc_spec
  ;;

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

  let to_completion_named_args { infos } = List.map infos ~f:Info.to_completion_named_arg
end

module Positional = struct
  type single_arg =
    { required : bool
    ; value_name : string
    ; completion : untyped_completion_hint option
    ; doc : string option
    }

  type all_above_inclusive =
    { index : int
    ; arg : single_arg
    }

  (* Keeps track of which indices of positional argument have parsers registered *)
  type t =
    { all_above_inclusive : all_above_inclusive option
    ; others_by_index : single_arg Int.Map.t
    }

  let empty = { all_above_inclusive = None; others_by_index = Int.Map.empty }

  let is_empty { all_above_inclusive; others_by_index } =
    Option.is_none all_above_inclusive && Int.Map.is_empty others_by_index
  ;;

  let command_doc_spec_of_single_arg { required; value_name; doc; _ }
    : Command_doc_spec.Positional_arg.t
    =
    let value = { Command_doc_spec.Value.name = value_name; required } in
    { Command_doc_spec.Positional_arg.value; doc }
  ;;

  let command_doc_spec { all_above_inclusive; others_by_index } =
    let fixed =
      Int.Map.bindings others_by_index
      |> List.map ~f:snd
      |> List.map ~f:command_doc_spec_of_single_arg
    in
    let repeated =
      Option.map all_above_inclusive ~f:(fun { arg; _ } ->
        command_doc_spec_of_single_arg arg)
    in
    { Command_doc_spec.Positional_args.fixed; repeated }
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
      let others_by_index =
        Int.Map.filter t.others_by_index ~f:(fun index { value_name; required; _ } ->
          if index >= all_above_inclusive.index
          then (
            check_value_names index value_name all_above_inclusive.arg.value_name;
            if required
            then Error.spec_error (Conflicting_requiredness_for_positional_argument index);
            false)
          else true)
      in
      { t with others_by_index }
  ;;

  let add_index t index ~value_name ~required ~completion ~doc =
    let others_by_index =
      Int.Map.update t.others_by_index ~key:index ~f:(function
        | None -> Some { value_name; required; completion; doc }
        | Some x ->
          check_value_names index x.value_name value_name;
          if x.required <> required
          then Error.spec_error (Conflicting_requiredness_for_positional_argument index);
          Some x)
    in
    trim_map { t with others_by_index }
  ;;

  let add_all_above_inclusive t index ~value_name ~completion ~doc =
    match t.all_above_inclusive with
    | Some x when x.index < index ->
      check_value_names index x.arg.value_name value_name;
      t
    | _ ->
      trim_map
        { t with
          all_above_inclusive =
            Some { index; arg = { required = false; value_name; completion; doc } }
        }
  ;;

  let add_all_below_exclusive t index ~value_name ~required ~completion ~doc =
    Seq.init index Fun.id
    |> Seq.fold_left (add_index ~value_name ~required ~completion ~doc) t
    |> trim_map
  ;;

  let merge x y =
    let all_above_inclusive =
      match x.all_above_inclusive, y.all_above_inclusive with
      | None, None -> None
      | Some a, None | None, Some a -> Some a
      | Some x, Some y ->
        check_value_names (Int.max x.index y.index) x.arg.value_name y.arg.value_name;
        let index = Int.min x.index y.index in
        Some { index; arg = x.arg }
    in
    let others_by_index =
      Int.Map.merge x.others_by_index y.others_by_index ~f:(fun index x y ->
        match x, y with
        | None, None -> None
        | Some value_name, None | None, Some value_name -> Some value_name
        | Some x, Some y ->
          check_value_names index x.value_name y.value_name;
          if x.required <> y.required
          then Error.spec_error (Conflicting_requiredness_for_positional_argument index);
          Some x)
    in
    trim_map { all_above_inclusive; others_by_index }
  ;;

  let single_at_index i = add_index empty i
  let all_above_inclusive i = add_all_above_inclusive empty i
  let all_below_exclusive i = add_all_below_exclusive empty i

  (* Check that there are no gaps in the declared positional arguments (E.g.
     if the parser would interpret the argument at position 0 and 2 but not 1
     it's probably an error.) *)
  let validate_no_gaps { all_above_inclusive; others_by_index } =
    let other_indices = Int.Map.to_seq others_by_index |> Seq.map fst |> Int.Set.of_seq in
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

  let to_completions ({ all_above_inclusive; others_by_index } as t) =
    if Result.is_error (validate_no_gaps t)
    then raise (Invalid_argument "positional argument spec has gaps");
    let finite_args =
      Int.Map.bindings others_by_index
      |> List.map ~f:(fun (_, { completion; _ }) -> completion)
    in
    let repeated_arg =
      Option.map all_above_inclusive ~f:(fun { arg = { completion; _ }; _ } ->
        match completion with
        | None -> `No_hint
        | Some hint -> `Hint hint)
    in
    { Completion_spec.Positional_args_hints.finite_args; repeated_arg }
  ;;

  let arg_count { all_above_inclusive; others_by_index } =
    match all_above_inclusive with
    | Some _ -> `Unlimited
    | None -> `Limited (Int.Map.cardinal others_by_index)
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

let create_flag names ~doc ~hidden ~repeated =
  create_named (Named.Info.flag names ~doc ~hidden ~repeated)
;;

let command_doc_spec { named; positional } =
  { Command_doc_spec.Args.named = Named.command_doc_spec named
  ; positional = Positional.command_doc_spec positional
  }
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
