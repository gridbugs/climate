open! Import
module Nonempty_list = Climate_stdlib.Nonempty_list

module Names = struct
  type t = Name.t Nonempty_list.t
end

module Command_line = struct
  let from_env () = Sys.argv |> Array.to_list |> List.tl
end

module Error = struct
  module Parse_error = struct
    (* Errors encountered while interpreting command-line arguments. This
       indicates that the user of a CLI program made with this library has
       passed invalid command-line arguments to the program. *)
    type t =
      | Opt_lacks_arg of Name.t
      | Flag_has_arg of { name : Name.t; value : string }
      | Opt_used_in_non_last_position_of_short_name_sequence of Name.t
      | No_such_name of Name.t
      | Invalid_name of Name.Invalid.t
      | Short_name_used_with_dash_dash of Name.t
      | Pos_req_missing of int
      | Opt_req_missing of Names.t
      | Opt_appeared_multiple_times of (Names.t * int)
      | Opt_req_appeared_multiple_times of (Names.t * int)
      | Flag_appeared_multiple_times of (Names.t * int)
      | Incomplete_command
      | Conv_failed of {
          locator : [ `Named of Name.t | `Positional of int ];
          message : string;
        }

    exception E of t
  end

  module Spec_error = struct
    (* Errors that indicate that a client of this library has attempted to create an invalid argument spec. *)
    type t =
      | Duplicate_name of Name.t
      | Invalid_name of Name.Invalid.t
      | Negative_position of int
      | Duplicate_enum_names of string list
      | No_such_enum_value of { valid_names : string list }

    exception E of t
  end

  module Implementation_error = struct
    (* Errors in the implementation of a parser. These are exposed so clients
       can implement custom parsers. *)
    type t =
      | No_such_name of Name.t
      | Not_a_flag of Name.t
      | Not_an_opt of Name.t

    exception E of t
  end
end

module Arg_table = struct
  module Spec = struct
    type arg_info = { has_arg : bool }
    type t = arg_info Name.Map.t

    let empty = Name.Map.empty

    let add t name ~has_arg =
      if Name.Map.mem name t then
        raise Error.Spec_error.(E (Duplicate_name name))
      else Name.Map.add t ~key:name ~data:{ has_arg }

    let add_names t names ~has_arg =
      List.fold_left (Nonempty_list.to_list names) ~init:t ~f:(fun acc name ->
          add acc name ~has_arg)

    let flag names = add_names empty names ~has_arg:false
    let opt names = add_names empty names ~has_arg:true

    let merge x y =
      Name.Map.fold y ~init:x ~f:(fun ~key ~data acc ->
          add acc key ~has_arg:data.has_arg)
  end

  type t = {
    spec : Spec.t;
    pos : string list;
    flag_counts : int Name.Map.t;
    opts : string list Name.Map.t;
  }

  let get_pos_all t = t.pos
  let get_pos t i = List.nth_opt t.pos i

  let get_flag_count t name =
    match Name.Map.find t.spec name with
    | None -> raise Error.(Implementation_error.E (No_such_name name))
    | Some { Spec.has_arg = true } ->
        raise Error.(Implementation_error.E (Not_a_flag name))
    | Some { has_arg = false } ->
        Name.Map.find t.flag_counts name |> Option.value ~default:0

  let get_opts t name =
    match Name.Map.find t.spec name with
    | None -> raise Error.(Implementation_error.E (No_such_name name))
    | Some { Spec.has_arg = false } ->
        raise Error.(Implementation_error.E (Not_an_opt name))
    | Some { has_arg = true } ->
        Name.Map.find t.opts name |> Option.value ~default:[]

  let get_flag_count_names t names =
    Nonempty_list.to_list names
    |> List.fold_left ~init:0 ~f:(fun acc name -> acc + get_flag_count t name)

  (* Returns a list of [Name.t * string] tuples associating values with the
     corresponding option names from the command line. This is to help with
     error messages since a single option may have multiple names and we want
     to print the specific name used to pass errorneous option values in
     error messages. *)
  let get_opts_names_by_name t names =
    Nonempty_list.to_list names
    |> List.concat_map ~f:(fun name ->
           get_opts t name |> List.map ~f:(fun value -> (name, value)))

  let empty spec =
    { spec; pos = []; flag_counts = Name.Map.empty; opts = Name.Map.empty }

  let add_opt t ~name ~value =
    { t with opts = Name.Map.add_to_list t.opts ~key:name ~data:value }

  let add_flag t ~name =
    {
      t with
      flag_counts =
        Name.Map.update t.flag_counts ~key:name ~f:(function
          | None -> Some 1
          | Some i -> Some (i + 1));
    }

  let add_pos t arg = { t with pos = arg :: t.pos }

  let reverse_lists t =
    { t with pos = List.rev t.pos; opts = Name.Map.map t.opts ~f:List.rev }

  let parse spec args =
    let rec parse_rec (acc : t) = function
      | [] -> Ok acc
      | "--" :: xs ->
          (* all arguments after a "--" argument are treated as positional *)
          Ok (List.fold_left xs ~init:acc ~f:add_pos)
      | "-" :: xs ->
          (* a "-" on its own is treated as positional *)
          let acc = add_pos acc "-" in
          parse_rec acc xs
      | x :: xs -> (
          match String.drop_prefix x ~prefix:"--" with
          | Some name_string -> (
              match String.lsplit2 name_string ~on:'=' with
              | Some (name_string, value) -> (
                  match Name.of_string name_string with
                  | Error invalid ->
                      Error (Error.Parse_error.Invalid_name invalid)
                  | Ok name -> (
                      match Name.kind name with
                      | `Short ->
                          Error
                            (Error.Parse_error.Short_name_used_with_dash_dash
                               name)
                      | `Long -> (
                          match Name.Map.find spec name with
                          | None -> Error (Error.Parse_error.No_such_name name)
                          | Some { Spec.has_arg = false } ->
                              Error
                                (Error.Parse_error.Flag_has_arg { name; value })
                          | Some { has_arg = true } ->
                              let acc = add_opt acc ~name ~value in
                              parse_rec acc xs)))
              | None -> (
                  match Name.of_string name_string with
                  | Error invalid ->
                      Error (Error.Parse_error.Invalid_name invalid)
                  | Ok name -> (
                      match Name.kind name with
                      | `Short ->
                          Error
                            (Error.Parse_error.Short_name_used_with_dash_dash
                               name)
                      | `Long -> (
                          match Name.Map.find spec name with
                          | None -> Error (Error.Parse_error.No_such_name name)
                          | Some { Spec.has_arg = false } ->
                              let acc = add_flag acc ~name in
                              parse_rec acc xs
                          | Some { has_arg = true } -> (
                              match xs with
                              | [] ->
                                  Error (Error.Parse_error.Opt_lacks_arg name)
                              | x :: xs ->
                                  let acc = add_opt acc ~name ~value:x in
                                  parse_rec acc xs)))))
          | None -> (
              match String.drop_prefix x ~prefix:"-" with
              | Some names -> (
                  match String.length names with
                  | 1 -> (
                      let name = Name.of_string_exn names in
                      match Name.Map.find spec name with
                      | None -> Error (Error.Parse_error.No_such_name name)
                      | Some { Spec.has_arg = false } ->
                          let acc = add_flag acc ~name in
                          parse_rec acc xs
                      | Some { has_arg = true } -> (
                          match xs with
                          | [] -> Error (Error.Parse_error.Opt_lacks_arg name)
                          | x :: xs ->
                              let acc = add_opt acc ~name ~value:x in
                              parse_rec acc xs))
                  | n -> (
                      assert (n > 1);
                      let name =
                        String.get names 1 |> String.make 1
                        |> Name.of_string_exn
                      in
                      match Name.Map.find spec name with
                      | None -> Error (Error.Parse_error.No_such_name name)
                      | Some { Spec.has_arg = true } ->
                          Error
                            (Error.Parse_error
                             .Opt_used_in_non_last_position_of_short_name_sequence
                               name)
                      | Some { has_arg = false } ->
                          let acc = add_flag acc ~name in
                          let rest = String.sub names ~pos:1 ~len:(n - 1) in
                          parse_rec acc (String.cat "-" rest :: xs)))
              | None ->
                  let acc = add_pos acc x in
                  parse_rec acc xs))
    in
    parse_rec (empty spec) args |> Result.map ~f:reverse_lists
end

module Term = struct
  type 'a parse = string -> ('a, [ `Msg of string ]) result
  type 'a print = Format.formatter -> 'a -> unit
  type 'a conv = { parse : 'a parse; print : 'a print }

  let sprintf = Printf.sprintf
  let string = { parse = Result.ok; print = Format.pp_print_string }

  let int =
    let parse s =
      match int_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an int)" s))
    in
    { parse; print = Format.pp_print_int }

  let float =
    let parse s =
      match float_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an float)" s))
    in
    { parse; print = Format.pp_print_float }

  let bool =
    let parse s =
      match bool_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an bool)" s))
    in
    { parse; print = Format.pp_print_bool }

  let enum l ~eq =
    let all_names = List.map l ~f:fst in
    let duplicate_names =
      List.fold_left all_names ~init:(String.Set.empty, [])
        ~f:(fun (set, duplicate_names) name ->
          if String.Set.mem name set then (set, name :: duplicate_names)
          else (String.Set.add name set, duplicate_names))
      |> snd |> List.rev
    in
    if List.length duplicate_names > 0 then
      raise Error.Spec_error.(E (Duplicate_enum_names duplicate_names));
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
            sprintf "invalid value: %S (valid values are: %s)" s
              all_names_string
          in
          Error (`Msg message)
    in
    let print ppf v =
      let name =
        List.find_map l ~f:(fun (name, value) ->
            if eq value v then Some name else None)
      in
      match name with
      | Some name -> Format.pp_print_string ppf name
      | None ->
          raise
            Error.Spec_error.(
              E (No_such_enum_value { valid_names = List.map l ~f:fst }))
    in
    { parse; print }

  type 'a arg_compute = Arg_table.t -> 'a
  type 'a t = { arg_spec : Arg_table.Spec.t; arg_compute : 'a arg_compute }
  type 'a nonempty_list = 'a Nonempty_list.t = ( :: ) of ('a * 'a list)

  let map { arg_spec; arg_compute } ~f =
    { arg_spec; arg_compute = (fun arg_table -> f (arg_compute arg_table)) }

  let both x y =
    {
      arg_spec = Arg_table.Spec.merge x.arg_spec y.arg_spec;
      arg_compute =
        (fun arg_table ->
          let x_value = x.arg_compute arg_table in
          let y_value = y.arg_compute arg_table in
          (x_value, y_value));
    }

  module O = struct
    let ( >>| ) t f = map t ~f
    let ( let+ ) = ( >>| )
    let ( and+ ) = both
  end

  let names_of_strings =
    Nonempty_list.map ~f:(fun string ->
        match Name.of_string string with
        | Ok name -> name
        | Error invalid -> raise Error.Spec_error.(E (Invalid_name invalid)))

  let opt_multi names conv =
    let names = names_of_strings names in
    {
      arg_spec = Arg_table.Spec.opt names;
      arg_compute =
        (fun arg_table ->
          Arg_table.get_opts_names_by_name arg_table names
          |> List.map ~f:(fun (name, value) ->
                 match conv.parse value with
                 | Ok value -> value
                 | Error (`Msg message) ->
                     raise
                       Error.Parse_error.(
                         E (Conv_failed { locator = `Named name; message }))));
    }

  let opt names conv =
    opt_multi names conv
    |> map ~f:(function
         | [] -> None
         | [ x ] -> Some x
         | many ->
             raise
               Error.Parse_error.(
                 E
                   (Opt_appeared_multiple_times
                      (names_of_strings names, List.length many))))

  let opt_req names conv =
    opt_multi names conv
    |> map ~f:(function
         | [] ->
             raise
               Error.Parse_error.(E (Opt_req_missing (names_of_strings names)))
         | [ x ] -> x
         | many ->
             raise
               Error.Parse_error.(
                 E
                   (Opt_req_appeared_multiple_times
                      (names_of_strings names, List.length many))))

  let flag_count names =
    let names = names_of_strings names in
    {
      arg_spec = Arg_table.Spec.flag names;
      arg_compute =
        (fun arg_table -> Arg_table.get_flag_count_names arg_table names);
    }

  let flag names =
    flag_count names
    |> map ~f:(function
         | 0 -> false
         | 1 -> true
         | n ->
             raise
               Error.Parse_error.(
                 E (Flag_appeared_multiple_times (names_of_strings names, n))))

  let pos i conv =
    let i =
      match Nonnegative_int.of_int i with
      | Some i -> i
      | None -> raise Error.Spec_error.(E (Negative_position i))
    in
    {
      arg_spec = Arg_table.Spec.empty;
      arg_compute =
        (fun arg_table ->
          Arg_table.get_pos arg_table (Nonnegative_int.to_int i)
          |> Option.map ~f:(fun x ->
                 match conv.parse x with
                 | Ok x -> x
                 | Error (`Msg message) ->
                     raise
                       Error.Parse_error.(
                         E
                           (Conv_failed
                              {
                                locator = `Positional (Nonnegative_int.to_int i);
                                message;
                              }))));
    }

  let pos_req i conv =
    pos i conv
    |> map ~f:(function
         | Some x -> x
         | None -> raise Error.Parse_error.(E (Pos_req_missing i)))

  let pos_all conv =
    {
      arg_spec = Arg_table.Spec.empty;
      arg_compute =
        (fun arg_table ->
          Arg_table.get_pos_all arg_table
          |> List.mapi ~f:(fun i x ->
                 match conv.parse x with
                 | Ok x -> x
                 | Error (`Msg message) ->
                     raise
                       Error.Parse_error.(
                         E (Conv_failed { locator = `Positional i; message }))));
    }

  let eval t command_line =
    let arg_table =
      match Arg_table.parse t.arg_spec command_line with
      | Ok x -> x
      | Error e -> raise (Error.Parse_error.E e)
    in
    t.arg_compute arg_table
end

module Command = struct
  type 'a t =
    | Singleton of 'a Term.t
    | Group of {
        children : (string * 'a t) list;
        default_term : 'a Term.t option;
      }

  let singleton term = Singleton term
  let group ?default_term children = Group { children; default_term }

  type 'a traverse = { term : 'a Term.t; command_line : string list }

  let rec traverse t command_line =
    match (t, command_line) with
    | Singleton term, command_line -> Ok { term; command_line }
    | Group { children; default_term }, x :: xs -> (
        let subcommand =
          List.find_map children ~f:(fun (name, command) ->
              if String.equal name x then Some command else None)
        in
        match subcommand with
        | Some subcommand -> traverse subcommand xs
        | None -> (
            match default_term with
            | Some term -> Ok { term; command_line = x :: xs }
            | None -> Error Error.Parse_error.Incomplete_command))
    | Group { children = _; default_term }, [] -> (
        match default_term with
        | Some term -> Ok { term; command_line = [] }
        | None -> Error Error.Parse_error.Incomplete_command)

  let eval t command_line =
    let { term; command_line } =
      match traverse t command_line with
      | Ok x -> x
      | Error e -> raise (Error.Parse_error.E e)
    in
    Term.eval term command_line

  let run t = Command_line.from_env () |> eval t
end
