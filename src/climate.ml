open! Import
module Nonempty_list = Climate_stdlib.Nonempty_list
module Parse_error = Error.Parse_error
module Spec_error = Error.Spec_error
module Command_line = Command_line

let name_of_string_exn string =
  match Name.of_string string with
  | Ok name -> name
  | Error e -> raise Spec_error.(E (Invalid_name (string, e)))
;;

module Arg_parser = struct
  module Context = struct
    type t =
      { raw_arg_table : Raw_arg_table.t
      ; command_line : Command_line.Rich.t
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

  let eval t ~(command_line : Command_line.Rich.t) ~ignore_errors =
    let raw_arg_table =
      match Raw_arg_table.parse t.arg_spec command_line.args ~ignore_errors with
      | Ok x -> x
      | Error e -> raise (Parse_error.E e)
    in
    let context = { Context.raw_arg_table; command_line } in
    t.arg_compute context
  ;;

  type 'a parse = string -> ('a, [ `Msg of string ]) result
  type 'a print = Format.formatter -> 'a -> unit

  module Completion = struct
    (* Roughly duplicated from [Spec.Untyped_completion.t] but
       with types that correspond to the type of the [conv] it will be
       part of. *)
    type _ t =
      | File : string t
      | Values : 'a list -> 'a t
      | Reentrant : (Command_line.Rich.t -> 'a list) -> 'a t

    let file = File
    let values values = Values values
    let reentrant f = Reentrant f

    let reentrant_parse parser =
      let f command_line = eval parser ~command_line ~ignore_errors:true in
      Reentrant f
    ;;

    let reentrant_thunk f =
      let f (_ : Command_line.Rich.t) = f () in
      Reentrant f
    ;;
  end

  type 'a conv =
    { parse : 'a parse
    ; print : 'a print
    ; default_value_name : string
    ; completion : 'a Completion.t option
    }

  let conv_value_to_string conv value =
    conv.print Format.str_formatter value;
    Format.flush_str_formatter ()
  ;;

  let conv_untyped_completion (type a) (conv : a conv) (completion : a Completion.t) =
    match completion with
    | File -> Completion_spec.Hint.File
    | Values values ->
      Completion_spec.Hint.Values (List.map values ~f:(conv_value_to_string conv))
    | Reentrant f ->
      Completion_spec.Hint.Reentrant
        (fun command_line -> f command_line |> List.map ~f:(conv_value_to_string conv))
  ;;

  (* A conv can have a built in completion, but it's also possible for
     this to be overridden for a specific parser. This is a helper
     function for converting a given completion, falling back to the
     built-in completion if none is given. *)
  let conv_untyped_completion_opt_with_default conv completion_opt =
    let completion_opt =
      if Option.is_some completion_opt then completion_opt else conv.completion
    in
    Option.map completion_opt ~f:(conv_untyped_completion conv)
  ;;

  let string =
    { parse = Result.ok
    ; print = Format.pp_print_string
    ; default_value_name = "STRING"
    ; completion = None
    }
  ;;

  let int =
    let parse s =
      match int_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an int)" s))
    in
    { parse; print = Format.pp_print_int; default_value_name = "INT"; completion = None }
  ;;

  let float =
    let parse s =
      match float_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an float)" s))
    in
    { parse
    ; print = Format.pp_print_float
    ; default_value_name = "FLOAT"
    ; completion = None
    }
  ;;

  let bool =
    let parse s =
      match bool_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (`Msg (sprintf "invalid value: %S (not an bool)" s))
    in
    { parse
    ; print = Format.pp_print_bool
    ; default_value_name = "BOOL"
    ; completion = Some (Completion.values [ true; false ])
    }
  ;;

  let file =
    { string with default_value_name = "FILE"; completion = Some Completion.file }
  ;;

  let enum ?(default_value_name = "VALUE") l ~eq =
    let all_names = List.map l ~f:fst in
    let all_values = List.map l ~f:snd in
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
    { parse; print; default_value_name; completion = Some (Completion.values all_values) }
  ;;

  let string_enum ?(default_value_name = "VALUE") l =
    enum ~default_value_name (List.map l ~f:(fun s -> s, s)) ~eq:String.equal
  ;;

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

  let names_of_strings strings =
    match Nonempty_list.of_list strings with
    | None -> raise Spec_error.(E Empty_name_list)
    | Some strings -> Nonempty_list.map strings ~f:name_of_string_exn
  ;;

  let const x = { arg_spec = Spec.empty; arg_compute = Fun.const x }
  let unit = const ()

  let program_name =
    { arg_spec = Spec.empty; arg_compute = (fun context -> context.command_line.program) }
  ;;

  let named_multi_gen info conv =
    { arg_spec = Spec.create_named info
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

  let named_opt_gen (info : Spec.Named.Info.t) conv ~allow_many =
    named_multi_gen info conv
    |> map ~f:(function
      | [] -> None
      | [ x ] -> Some x
      | x :: _ as many ->
        if allow_many
        then Some x
        else
          raise
            Parse_error.(
              E (Named_opt_appeared_multiple_times (info.names, List.length many))))
  ;;

  let named_multi ?desc ?value_name ?hidden ?completion names conv =
    named_multi_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = false
      ; desc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
      }
      conv
  ;;

  (* Like [named_opt] but takes its arguments as [Name.t]s rather than
     as strings is it's intended for use within this library. *)
  let named_opt_for_internal ?desc ?value_name ?hidden ?completion names conv =
    named_opt_gen
      { names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = false
      ; desc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
      }
      conv
  ;;

  let named_opt ?desc ?value_name ?hidden ?completion names conv =
    named_opt_for_internal
      ?desc
      ?value_name
      ?hidden
      ?completion
      (names_of_strings names)
      conv
      ~allow_many:false
  ;;

  let named_with_default_gen
    ?desc
    ?value_name
    ?hidden
    ?completion
    names
    conv
    ~default
    ~allow_many
    =
    named_opt_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = Some (conv_value_to_string conv default)
      ; required = false
      ; desc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
      }
      conv
      ~allow_many
    >>| Option.value ~default
  ;;

  let named_with_default = named_with_default_gen ~allow_many:false

  let named_req ?desc ?value_name ?hidden ?completion names conv =
    named_multi_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = true
      ; desc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
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

  let flag_count ?desc ?hidden names =
    let names = names_of_strings names in
    { arg_spec = Spec.create_flag names ~desc ~hidden:(Option.value hidden ~default:false)
    ; arg_compute =
        (fun context -> Raw_arg_table.get_flag_count_names context.raw_arg_table names)
    }
  ;;

  let flag_gen ?desc names ~allow_many =
    flag_count ?desc names
    |> map ~f:(function
      | 0 -> false
      | 1 -> true
      | n ->
        if allow_many
        then true
        else
          raise Parse_error.(E (Flag_appeared_multiple_times (names_of_strings names, n))))
  ;;

  let flag = flag_gen ~allow_many:false

  let pos_single_gen i conv ~value_name ~required ~completion =
    let i =
      match Nonnegative_int.of_int i with
      | Some _ -> i
      | None -> raise Spec_error.(E (Negative_position i))
    in
    { arg_spec =
        Spec.create_positional
          (Spec.Positional.single_at_index
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~required
             ~completion:(conv_untyped_completion_opt_with_default conv completion))
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

  let pos_opt ?value_name ?completion i conv =
    pos_single_gen i conv ~value_name ~required:false ~completion
  ;;

  let pos_req ?value_name ?completion i conv =
    pos_single_gen i conv ~value_name ~required:true ~completion
    |> map ~f:(function
      | Some x -> x
      | None -> raise Parse_error.(E (Pos_req_missing i)))
  ;;

  let pos_left_gen i conv ~value_name ~required ~completion =
    { arg_spec =
        Spec.create_positional
          (Spec.Positional.all_below_exclusive
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~required
             ~completion:(conv_untyped_completion_opt_with_default conv completion))
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

  let pos_left ?value_name ?completion i conv =
    pos_left_gen i conv ~value_name ~required:false ~completion
  ;;

  let pos_right ?value_name ?completion i conv =
    { arg_spec =
        Spec.create_positional
          (Spec.Positional.all_above_inclusive
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~completion:(conv_untyped_completion_opt_with_default conv completion))
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

  let pos_all ?value_name ?completion conv = pos_right ?value_name ?completion 0 conv
  let validate t = Spec.validate t.arg_spec

  let pp_help ppf arg_spec ~subcommand =
    Format.pp_print_string ppf "Usage:";
    List.iter subcommand ~f:(fun part -> Format.fprintf ppf " %s" part);
    Spec.usage ppf arg_spec;
    Format.pp_print_newline ppf ();
    Format.pp_print_newline ppf ();
    Spec.named_help ppf arg_spec
  ;;

  let add_help { arg_spec; arg_compute } =
    let help_spec =
      Spec.create_flag Built_in.help_names ~desc:(Some "Print help") ~hidden:false
    in
    let arg_spec = Spec.merge arg_spec help_spec in
    { arg_spec
    ; arg_compute =
        (fun context ->
          if Raw_arg_table.get_flag_count_names context.raw_arg_table Built_in.help_names
             > 0
          then (
            pp_help
              Format.std_formatter
              arg_spec
              ~subcommand:context.command_line.subcommand;
            exit 0)
          else arg_compute context)
    }
  ;;

  let finalize t =
    validate t;
    add_help t
  ;;

  module Reentrant = struct
    let unit = unit
    let map = map
    let both = both
    let ( >>| ) = ( >>| )
    let ( let+ ) = ( let+ )
    let ( and+ ) = ( and+ )
    let named_multi names conv = named_multi names conv

    let named_opt names conv =
      named_opt_for_internal (names_of_strings names) conv ~allow_many:true
    ;;

    let named_with_default names conv = named_with_default_gen names conv ~allow_many:true
    let flag_count names = flag_count names
    let flag names = flag_gen names ~allow_many:true
    let pos_opt i conv = pos_opt i conv
    let pos_all conv = pos_all conv
    let pos_left i conv = pos_left i conv
    let pos_right i conv = pos_right i conv
  end
end

module Completion_config = struct
  type t =
    { program_name : string
    ; program_exe : string
    }

  (* An internal argument parser accepting arguments for configuring
     how the completion script is printed *)
  let arg_parser =
    let open Arg_parser in
    Arg_parser.finalize
      (let+ program_name =
         named_opt
           ~desc:
             "Name to register this completion script with in the shell. Should be the \
              name of this program's executable. Will default to argv[0]."
           ~value_name:"PROGRAM"
           [ "program-name" ]
           string
       and+ program_exe =
         named_opt
           ~desc:
             "Program to run when executing reentrant queries. This should usually be \
              the same as program-name. Will default to argv[0]."
           ~value_name:"PROGRAM"
           [ "program-exe" ]
           string
       in
       let program_name =
         match program_name with
         | Some program_name -> program_name
         | None -> Sys.argv.(0)
       in
       let program_exe =
         match program_exe with
         | Some program_exe -> program_exe
         | None -> Sys.argv.(0)
       in
       { program_name; program_exe })
  ;;
end

module Eval_config = struct
  type t = { print_reentrant_completions_name : Name.t }

  let default =
    { print_reentrant_completions_name =
        Name.of_string_exn "print-reentrant-completion-hints"
    }
  ;;
end

module Command = struct
  type internal = Print_completion_script_bash

  module Info = struct
    type t =
      { name : Name.t
      ; hidden : bool
      }
  end

  type 'a t =
    | Singleton of 'a Arg_parser.t
    | Group of
        { children : 'a subcommand list
        ; default_arg_parser : 'a Arg_parser.t option
        }
    | Internal of internal

  and 'a subcommand =
    { info : Info.t
    ; command : 'a t
    }

  let singleton term = Singleton (Arg_parser.finalize term)

  let subcommand ?(hidden = false) name_string command =
    { info = { Info.name = name_of_string_exn name_string; hidden }; command }
  ;;

  let group ?default_arg_parser children =
    let default_arg_parser = Option.map default_arg_parser ~f:Arg_parser.finalize in
    Group { children; default_arg_parser }
  ;;

  let print_completion_script_bash = Internal Print_completion_script_bash

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
        List.find_map children ~f:(fun { info = { name; _ }; command } ->
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

  let rec completion_spec = function
    | Singleton arg_parser ->
      let parser_spec = Spec.to_completion_parser_spec arg_parser.arg_spec in
      { Completion_spec.parser_spec; subcommands = [] }
    | Internal Print_completion_script_bash ->
      let parser_spec =
        Spec.to_completion_parser_spec Completion_config.arg_parser.arg_spec
      in
      { Completion_spec.parser_spec; subcommands = [] }
    | Group { children; default_arg_parser } ->
      let parser_spec =
        match default_arg_parser with
        | Some default_arg_parser ->
          Spec.to_completion_parser_spec default_arg_parser.arg_spec
        | None -> Completion_spec.Parser_spec.empty
      in
      let subcommands =
        List.filter_map children ~f:(fun { info; command } ->
          if info.hidden
          then None
          else (
            let spec = completion_spec command in
            Some { Completion_spec.name = Name.to_string info.name; spec }))
      in
      { Completion_spec.parser_spec; subcommands }
  ;;

  let completion_script_bash
    ?(eval_config = Eval_config.default)
    t
    ~program_name
    ~program_exe
    =
    completion_spec t
    |> Completion.generate_bash
         ~print_reentrant_completions_name:eval_config.print_reentrant_completions_name
         ~program_name
         ~program_exe
  ;;

  module Reentrant_query = struct
    type t =
      { index : int
      ; command_line : Command_line.Raw.t
      }

    (* An internal argument parser accepting a reentrant function index
       and a partial command to parse to the corresponding reentrant
       function. *)
    let arg_parser name =
      let open Arg_parser in
      let+ index = named_opt_for_internal [ name ] int ~allow_many:true
      and+ command_line = pos_all string in
      Option.map index ~f:(fun index ->
        if List.is_empty command_line
        then
          failwith
            "reentrant query was invoked with no positional arguments, which the \
             completion script should never do";
        match command_line with
        | [] -> failwith "unexpected empty list"
        | program :: args ->
          let command_line = { Command_line.Raw.program; args } in
          { index; command_line })
    ;;

    (* Evaluate this type's argument parser on a given argument list. *)
    let eval_arg_parser name (raw_command_line : Command_line.Raw.t) =
      match raw_command_line.args with
      | [] -> None
      | _ ->
        let command_line =
          { Command_line.Rich.program = raw_command_line.program
          ; args = raw_command_line.args
          ; subcommand = []
          }
        in
        Arg_parser.eval (arg_parser name) ~command_line ~ignore_errors:true
    ;;

    let run_query t command completion_spec =
      let all_reentrants = Completion_spec.all_reentrants completion_spec in
      match List.nth_opt all_reentrants t.index with
      | Some reentrant ->
        let subcommand, args =
          match traverse command t.command_line.args [] with
          | Ok { subcommand; args; _ } -> subcommand, args
          | Error _ -> [], t.command_line.args
        in
        reentrant { Command_line.Rich.program = t.command_line.program; subcommand; args }
      | None ->
        failwith
          "reentrant query was invoked with an out of bounds argument, which the \
           completion script should never do"
    ;;
  end

  let eval ?(eval_config = Eval_config.default) t (raw_command_line : Command_line.Raw.t) =
    let completion_spec = completion_spec t in
    (* If the top-level command was passed the reentrant query
       argument, cancel normal operation and just invoke the appropriate
       reentrant function. *)
    (match
       Reentrant_query.eval_arg_parser
         eval_config.print_reentrant_completions_name
         raw_command_line
     with
     | Some reentrant_query ->
       let reentrant_suggestions =
         Reentrant_query.run_query reentrant_query t completion_spec
       in
       List.iter reentrant_suggestions ~f:print_endline;
       exit 0
     | None -> ());
    let { operation; args; subcommand } =
      match traverse t raw_command_line.args [] with
      | Ok x -> x
      | Error e -> raise (Parse_error.E e)
    in
    let command_line =
      { Command_line.Rich.program = raw_command_line.program; args; subcommand }
    in
    match operation with
    | `Arg_parser arg_parser ->
      (* This is the common case. Run the selected argument parser
         which will usually have the side effect of running the user's
         program logic. *)
      Arg_parser.eval arg_parser ~command_line ~ignore_errors:false
    | `Internal Print_completion_script_bash ->
      (* Print the completion script. Note that this can't be combined
         into the regular parser logic because it needs to be the
         completion spec, which isn't available to regular argument
         parsers. *)
      let { Completion_config.program_name; program_exe } =
        Arg_parser.eval Completion_config.arg_parser ~command_line ~ignore_errors:false
      in
      print_endline
        (Completion.generate_bash
           completion_spec
           ~program_name
           ~program_exe
           ~print_reentrant_completions_name:eval_config.print_reentrant_completions_name);
      exit 0
  ;;

  let run ?(eval_config = Eval_config.default) t =
    try Command_line.Raw.from_env () |> eval ~eval_config t with
    | Parse_error.E e ->
      Printf.eprintf "%s" (Parse_error.to_string e);
      exit Parse_error.exit_code
  ;;
end

module For_test = struct
  include For_test
  module Parse_error = Parse_error
end
