open! Import

module Hint = struct
  type 'reentrant t =
    | File
    | Values of string list
    | Reentrant of 'reentrant

  let replace_reentrants_with_indices t acc =
    match t with
    | Reentrant _ -> Reentrant acc, acc + 1
    | Values v -> Values v, acc
    | File -> File, acc
  ;;

  let all_reentrants = function
    | Reentrant r -> [ r ]
    | _ -> []
  ;;
end

module Named_arg = struct
  type 'reentrant t =
    { name : Name.t
    ; has_param : bool
    ; hint : 'reentrant Hint.t option
    }

  let replace_reentrants_with_indices t acc =
    match t.hint with
    | Some hint ->
      let hint, acc = Hint.replace_reentrants_with_indices hint acc in
      { t with hint = Some hint }, acc
    | None -> { t with hint = None }, acc
  ;;

  let all_reentrants t =
    match t.hint with
    | Some hint -> Hint.all_reentrants hint
    | None -> []
  ;;
end

module Positional_args_hints = struct
  type 'reentrant t =
    { finite_args : 'reentrant Hint.t option list
    ; repeated_arg : [ `No_hint | `Hint of 'reentrant Hint.t ] option
    }

  let empty = { finite_args = []; repeated_arg = None }

  let replace_reentrants_with_indices t acc =
    let finite_args, acc =
      List.fold_left t.finite_args ~init:([], acc) ~f:(fun (hints, acc) ->
          function
          | Some hint ->
            let hint, acc = Hint.replace_reentrants_with_indices hint acc in
            Some hint :: hints, acc
          | None -> None :: hints, acc)
    in
    let finite_args = List.rev finite_args in
    let repeated_arg, acc =
      match t.repeated_arg with
      | Some (`Hint hint) ->
        let hint, acc = Hint.replace_reentrants_with_indices hint acc in
        Some (`Hint hint), acc
      | Some `No_hint -> Some `No_hint, acc
      | None -> None, acc
    in
    { finite_args; repeated_arg }, acc
  ;;

  let all_reentrants t =
    let from_finite_args =
      List.concat_map t.finite_args ~f:(function
        | Some hint -> Hint.all_reentrants hint
        | None -> [])
    in
    let from_repeated_arg =
      match t.repeated_arg with
      | Some (`Hint hint) -> Hint.all_reentrants hint
      | _ -> []
    in
    from_finite_args @ from_repeated_arg
  ;;
end

module Parser_spec = struct
  type 'reentrant t =
    { named_args : 'reentrant Named_arg.t list
    ; positional_args_hints : 'reentrant Positional_args_hints.t
    }

  let empty = { named_args = []; positional_args_hints = Positional_args_hints.empty }

  let replace_reentrants_with_indices t acc =
    let named_args, acc =
      List.fold_left t.named_args ~init:([], acc) ~f:(fun (named_args, acc) named_arg ->
        let named_arg, acc = Named_arg.replace_reentrants_with_indices named_arg acc in
        named_arg :: named_args, acc)
    in
    let named_args = List.rev named_args in
    let positional_args_hints, acc =
      Positional_args_hints.replace_reentrants_with_indices t.positional_args_hints acc
    in
    { named_args; positional_args_hints }, acc
  ;;

  let all_reentrants t =
    let from_named_args = List.concat_map t.named_args ~f:Named_arg.all_reentrants in
    let from_positional_args_hints =
      Positional_args_hints.all_reentrants t.positional_args_hints
    in
    from_named_args @ from_positional_args_hints
  ;;
end

type 'reentrant t =
  { parser_spec : 'reentrant Parser_spec.t
  ; subcommands : 'reentrant subcommand list
  }

and 'reentrant subcommand =
  { name : string
  ; spec : 'reentrant t
  }

let empty = { parser_spec = Parser_spec.empty; subcommands = [] }

let named_args_sorted { parser_spec = { named_args; _ }; _ } =
  let cmp (arg1 : _ Named_arg.t) (arg2 : _ Named_arg.t) =
    String.compare (Name.to_string arg1.name) (Name.to_string arg2.name)
  in
  let long_args =
    List.filter named_args ~f:(fun { Named_arg.name; _ } -> Name.is_long name)
    |> List.sort ~cmp
  in
  let short_args =
    List.filter named_args ~f:(fun { Named_arg.name; _ } -> Name.is_short name)
    |> List.sort ~cmp
  in
  long_args @ short_args
;;

let rec replace_reentrants_with_indices t acc =
  let parser_spec, acc = Parser_spec.replace_reentrants_with_indices t.parser_spec acc in
  let subcommands, acc =
    List.fold_left
      t.subcommands
      ~init:([], acc)
      ~f:(fun (subcommands, acc) { name; spec } ->
        let spec, acc = replace_reentrants_with_indices spec acc in
        { name; spec } :: subcommands, acc)
  in
  let subcommands = List.rev subcommands in
  { parser_spec; subcommands }, acc
;;

let replace_reentrants_with_indices t = replace_reentrants_with_indices t 0 |> fst

let rec all_reentrants t =
  let from_parser_spec = Parser_spec.all_reentrants t.parser_spec in
  let from_subcommands =
    List.concat_map t.subcommands ~f:(fun { spec; _ } -> all_reentrants spec)
  in
  from_parser_spec @ from_subcommands
;;
