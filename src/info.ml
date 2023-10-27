open! Import

module Term = struct
  module Names = struct
    type t = Name.t Nonempty_list.t

    let of_strings name_strings =
      List.map name_strings ~f:Name.of_string
      |> Result.List.all
      |> Result.bind ~f:(fun names ->
             match List.find_duplicate ~eq:Name.equal names with
             | Some duplicate_name -> Error (`Duplicate_name duplicate_name)
             | None -> Ok names)
      |> Result.bind ~f:(fun names ->
             match Nonempty_list.of_list names with
             | None -> Error `No_names
             | Some names -> Ok names)
  end

  module Locator = struct
    type t =
      | Named of Names.t
      | At_position of Nonnegative_int.t
      | All_positional

    let named_of_strings strings =
      Names.of_strings strings |> Result.map ~f:(fun names -> Named names)

    let at_position_of_int int =
      match Nonnegative_int.of_int int with
      | Some position -> Ok (At_position position)
      | None -> Error (`Negative_position int)
  end

  type t = { locator : Locator.t }

  let create locator = { locator }

  let names { locator } =
    match locator with
    | Named names -> Nonempty_list.to_list names
    | At_position _ | All_positional -> []

  module Sealed = struct
    type info = t
    type nonrec t = { infos : info list; by_name : info Name.Map.t }

    let infos_by_name_result infos =
      List.concat_map infos ~f:(fun info ->
          List.map (names info) ~f:(fun name -> (name, info)))
      |> Name.Map.of_list
      |> Result.map_error ~f:(fun (duplicate_name, info1, info2) ->
             `Duplicate_name_across_terms
               (duplicate_name, names info1, names info2))

    (* Check that there are no gaps in the position argument indices. *)
    let validate_positionals infos =
      let contains_all_positional =
        List.exists infos ~f:(fun info ->
            match info.locator with
            | All_positional -> true
            | At_position _ | Named _ -> false)
      in
      if contains_all_positional then Ok ()
      else
        let all_position_indices =
          List.filter_map infos ~f:(fun info ->
              match info.locator with
              | At_position i -> Some (Nonnegative_int.to_int i)
              | All_positional | Named _ -> None)
          |> Int.Set.of_list
        in
        if Int.Set.is_empty all_position_indices then Ok ()
        else
          let max_index = Int.Set.max_elt all_position_indices in
          List.init ~len:max_index ~f:(fun i ->
              if Int.Set.mem i all_position_indices then Ok ()
              else Error (`Gap_in_positional_indices (`Gap i, `Max max_index)))
          |> Result.List.all
          |> Result.map ~f:(Fun.const ())

    let create infos =
      Result.bind (infos_by_name_result infos) ~f:(fun by_name ->
          Result.map (validate_positionals infos) ~f:(fun () ->
              { infos; by_name }))

    let infos t = t.infos
    let infos_by_name t = t.by_name
  end
end
