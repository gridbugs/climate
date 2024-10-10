open! Import
module Parse_error = Error.Parse_error

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
  | None -> failwith (sprintf "no such arg %s" (Name.to_string_with_dashes name))
  | Some info ->
    if Spec.Named.Info.has_param info
    then
      failwith
        (sprintf "arg %s exists but is not a flag" (Name.to_string_with_dashes name))
    else Name.Map.find t.flag_counts name |> Option.value ~default:0
;;

let get_opts t name =
  match Spec.Named.get_info_by_name t.spec.named name with
  | None -> failwith (sprintf "no such arg %s" (Name.to_string_with_dashes name))
  | Some info ->
    if Spec.Named.Info.has_param info
    then Name.Map.find t.opts name |> Option.value ~default:[]
    else
      failwith
        (sprintf
           "arg %s exists but does not take a value"
           (Name.to_string_with_dashes name))
;;

let get_flag_count_names t names =
  Nonempty_list.to_list names
  |> List.fold_left ~init:0 ~f:(fun acc name -> acc + get_flag_count t name)
;;

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

let add_pos t arg ~ignore_errors =
  let ret = { t with pos = arg :: t.pos } in
  match Spec.Positional.arg_count t.spec.positional with
  | `Unlimited -> Ok ret
  | `Limited count ->
    if List.length ret.pos > count
    then
      if ignore_errors
      then (* Return the table unchangned. *)
        Ok t
      else Error (Parse_error.Too_many_positional_arguments { max = count })
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
let parse_short_sequence t short_sequence remaining_args ~ignore_errors =
  let open Result.O in
  let rec loop acc remaining_short_sequence remaining_args =
    let result =
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
    if ignore_errors
    then (
      match result with
      | Ok _ -> result
      | Error _ ->
        (match Name.chip_short_name_off_string remaining_short_sequence with
         | Error _ ->
           (* Give up, keeping the result so far. *)
           Ok (acc, remaining_args)
         | Ok (_, rest) ->
           (* Continue parsing the short sequence after skipping the first name. *)
           loop acc rest remaining_args))
    else result
  in
  loop t short_sequence remaining_args
;;

let parse (spec : Spec.t) args ~ignore_errors =
  let open Result.O in
  let rec loop (acc : t) = function
    | [] -> Ok acc
    | "--" :: xs ->
      (* all arguments after a "--" argument are treated as positional *)
      Result.List.fold_left (List.rev xs) ~init:acc ~f:(add_pos ~ignore_errors)
    | "-" :: xs ->
      (* a "-" on its own is treated as positional *)
      Result.bind (add_pos acc "-" ~ignore_errors) ~f:(fun acc -> loop acc xs)
    | x :: xs ->
      (match String.drop_prefix x ~prefix:"--" with
       | Some name_string ->
         (match parse_long_name acc name_string xs with
          | Ok (acc, xs) -> loop acc xs
          | Error e ->
            if ignore_errors
            then
              (* Keep trying to parse the command, ignoring the problematic value *)
              loop acc xs
            else Error e)
       | None ->
         (* x doesn't begin with "--" *)
         (match String.drop_prefix x ~prefix:"-" with
          | Some short_sequence ->
            let* acc, xs = parse_short_sequence acc short_sequence xs ~ignore_errors in
            loop acc xs
          | None -> Result.bind (add_pos acc x ~ignore_errors) ~f:(fun acc -> loop acc xs)))
  in
  loop (empty spec) args |> Result.map ~f:reverse_lists
;;
