open! Import
module Parse_error = Error.Parse_error
module Spec_error = Error.Spec_error

module Help_style = struct
  include Help.Style

  type color =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    | `Bright_black
    | `Bright_red
    | `Bright_green
    | `Bright_yellow
    | `Bright_blue
    | `Bright_magenta
    | `Bright_cyan
    | `Bright_white
    ]

  type ansi_style = Ansi_style.t =
    { bold : bool
    ; dim : bool
    ; underline : bool
    ; color : color option
    }

  let ansi_style_plain = Ansi_style.default
end

module Manpage = struct
  include Manpage

  type markup =
    [ `P of string
    | `Pre of string
    ]

  type prose = Manpage.Prose.t

  let prose = Prose.create
end

let name_of_string_exn string =
  match Name.of_string string with
  | Ok name -> name
  | Error e -> Error.spec_error (Invalid_name (string, e))
;;

module Subcommand = struct
  type t =
    { name : Name.t
    ; aliases : Name.t list
    ; doc : string option
    ; arg_spec : Spec.t
    }

  let command_doc_spec { name; aliases; doc; arg_spec } =
    let args = Spec.command_doc_spec arg_spec in
    { Command_doc_spec.Subcommand.name; aliases; doc; args }
  ;;
end

module Arg_parser = struct
  module Completion_ = Completion

  module Context = struct
    type t =
      { raw_arg_table : Raw_arg_table.t
      ; command_line : Command_line.Rich.t
      }
  end

  type 'a arg_compute = Context.t -> ('a, Non_ret.t) result

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
    let open Result.O in
    let* raw_arg_table =
      Raw_arg_table.parse t.arg_spec command_line.args ~ignore_errors
      |> Result.map_error ~f:(fun e -> Non_ret.Parse_error e)
    in
    let context = { Context.raw_arg_table; command_line } in
    t.arg_compute context
  ;;

  type 'a parse = string -> ('a, [ `Msg of string ]) result
  type 'a print = Format.formatter -> 'a -> unit

  let to_string_print to_string fmt value = Format.pp_print_string fmt (to_string value)

  let value_to_string print value =
    print Format.str_formatter value;
    Format.flush_str_formatter ()
  ;;

  module Completion = struct
    type command_line = Command_line.Rich.t =
      { program : string
      ; subcommand : string list
      ; args : string list
      }

    type 'a t =
      | File
      | Strings of string list
      | Strings_reentrant of (command_line -> string list)
      | Values of 'a list
      | Values_reentrant of (command_line -> 'a list)

    let file = File
    let values values = Values values
    let reentrant f = Values_reentrant f

    let reentrant_parse parser =
      let f command_line =
        match eval parser ~command_line ~ignore_errors:true with
        | Ok value -> value
        | Error _ -> failwith "Reentrant parser did not yield a result"
      in
      Values_reentrant f
    ;;

    let reentrant_thunk f =
      let f _ = f () in
      Values_reentrant f
    ;;

    let map t ~f =
      match t with
      | (File | Strings _ | Strings_reentrant _) as t' -> t'
      | Values xs -> Values (List.map ~f xs)
      | Values_reentrant get_suggestions ->
        Values_reentrant
          (fun command_line ->
            let suggestions = get_suggestions command_line in
            List.map ~f suggestions)
    ;;

    let some t = map t ~f:Option.some

    let stringify t print =
      match t with
      | (File | Strings _ | Strings_reentrant _) as t' -> t'
      | Values xs -> Strings (List.map ~f:(value_to_string print) xs)
      | Values_reentrant get_suggestions ->
        Strings_reentrant
          (fun command_line ->
            let suggestions = get_suggestions command_line in
            List.map ~f:(value_to_string print) suggestions)
    ;;
  end

  type 'a conv =
    { parse : 'a parse
    ; print : 'a print
    ; default_value_name : string
    ; completion : 'a Completion.t option
    }

  let make_conv ~parse ~print ?(default_value_name = "VALUE") ?(completion = None) () =
    { parse; print; default_value_name; completion }
  ;;

  let conv_untyped_completion print completion =
    match (completion : _ Completion.t) with
    | File -> Completion_spec.Hint.File
    | Strings strings -> Completion_spec.Hint.Values strings
    | Strings_reentrant f -> Completion_spec.Hint.Reentrant f
    | Values values ->
      Completion_spec.Hint.Values (List.map values ~f:(value_to_string print))
    | Values_reentrant f ->
      Completion_spec.Hint.Reentrant
        (fun command_line ->
          let suggestions = f command_line in
          List.map suggestions ~f:(value_to_string print))
  ;;

  (* A conv can have a built-in completion, but it's also possible for
     this to be overridden for a specific parser. This is a helper
     function for converting a given completion, falling back to the
     built-in completion if none is given. *)
  let conv_untyped_completion_opt_with_default conv completion_opt =
    let completion_opt =
      if Option.is_some completion_opt then completion_opt else conv.completion
    in
    Option.map completion_opt ~f:(conv_untyped_completion conv.print)
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

  let enum ?(default_value_name = "VALUE") ?(eq = ( = )) l =
    let all_names = List.map l ~f:fst in
    let all_values = List.map l ~f:snd in
    let duplicate_names = String.find_duplicates all_names in
    if List.length duplicate_names > 0
    then Error.spec_error (Duplicate_enum_names duplicate_names);
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
      | None -> Error.spec_error (No_such_enum_value { valid_names = List.map l ~f:fst })
    in
    { parse; print; default_value_name; completion = Some (Completion.values all_values) }
  ;;

  let string_enum ?(default_value_name = "VALUE") l =
    enum ~default_value_name (List.map l ~f:(fun s -> s, s)) ~eq:String.equal
  ;;

  let pair ?(sep = ',') a b =
    let parse string =
      match String.split_on_char ~sep string with
      | [] | [ _ ] -> Error (`Msg (sprintf "No separator (%c) found in %S" sep string))
      | x :: xs ->
        let rest = String.concat ~sep:(String.make 1 sep) xs in
        Result.bind (a.parse x) ~f:(fun ax ->
          Result.map (b.parse rest) ~f:(fun bx -> ax, bx))
    in
    let print ppf (ax, bx) =
      a.print ppf ax;
      Format.pp_print_char ppf sep;
      b.print ppf bx
    in
    { parse; print; default_value_name = "PAIR"; completion = None }
  ;;

  let list ?(sep = ',') element_conv =
    let parse string =
      Result.List.all (String.split_on_char ~sep string |> List.map ~f:element_conv.parse)
    in
    let print ppf elements =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_char ppf sep)
        element_conv.print
        ppf
        elements
    in
    { parse; print; default_value_name = "LIST"; completion = None }
  ;;

  type 'a nonempty_list = 'a Nonempty_list.t = ( :: ) of ('a * 'a list)

  let map { arg_spec; arg_compute } ~f =
    { arg_spec; arg_compute = (fun context -> Result.map ~f (arg_compute context)) }
  ;;

  let map' { arg_spec; arg_compute } ~f =
    { arg_spec; arg_compute = (fun context -> Result.bind ~f (arg_compute context)) }
  ;;

  let both x y =
    { arg_spec = Spec.merge x.arg_spec y.arg_spec
    ; arg_compute =
        (fun context ->
          let open Result.O in
          let+ x_value = x.arg_compute context
          and+ y_value = y.arg_compute context in
          x_value, y_value)
    }
  ;;

  let ( >>| ) t f = map t ~f
  let ( let+ ) = ( >>| )
  let ( and+ ) = both

  let apply f x =
    let+ f = f
    and+ x = x in
    f x
  ;;

  let ( $ ) f x = apply f x

  let names_of_strings strings =
    match Nonempty_list.of_list strings with
    | None -> Error.spec_error Empty_name_list
    | Some strings -> Nonempty_list.map strings ~f:name_of_string_exn
  ;;

  let const x = { arg_spec = Spec.empty; arg_compute = (fun _context -> Ok x) }
  let unit = const ()

  let argv0 =
    { arg_spec = Spec.empty
    ; arg_compute = (fun context -> Ok context.command_line.program)
    }
  ;;

  let last t =
    map' t ~f:(fun list ->
      match List.last list with
      | None ->
        Error
          (Non_ret.Parse_error
             (Parse_error.Conv_failed
                { locator = None; message = "Unexpected empty list" }))
      | Some x -> Ok x)
  ;;

  let named_multi_gen info conv =
    { arg_spec = Spec.create_named info
    ; arg_compute =
        (fun context ->
          Raw_arg_table.get_opts_names_by_name context.raw_arg_table info.names
          |> List.map ~f:(fun (name, value) ->
            conv.parse value
            |> Result.map_error ~f:(fun (`Msg message) ->
              Non_ret.Parse_error
                (Parse_error.Conv_failed { locator = Some (`Named name); message })))
          |> Result.List.all)
    }
  ;;

  let named_opt_gen (info : Spec.Named.Info.t) conv ~allow_many =
    named_multi_gen info conv
    |> map' ~f:(function
      | [] -> Ok None
      | [ x ] -> Ok (Some x)
      | x :: _ as many ->
        if allow_many
        then Ok (Some x)
        else
          Error
            (Non_ret.Parse_error
               (Parse_error.Named_opt_appeared_multiple_times
                  (info.names, List.length many))))
  ;;

  let named_multi ?doc ?value_name ?hidden ?completion names conv =
    named_multi_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = false
      ; doc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
      ; repeated = true
      }
      conv
  ;;

  (* Like [named_opt] but takes its arguments as [Name.t]s rather than
     as strings is it's intended for use within this library. *)
  let named_opt_for_internal ?doc ?value_name ?hidden ?completion names conv =
    named_opt_gen
      { names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = false
      ; doc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
      ; repeated = false
      }
      conv
  ;;

  let named_opt ?doc ?value_name ?hidden ?completion names conv =
    named_opt_for_internal
      ?doc
      ?value_name
      ?hidden
      ?completion
      (names_of_strings names)
      conv
      ~allow_many:false
  ;;

  let named_with_default_gen
    ?doc
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
      ; default_string = Some (value_to_string conv.print default)
      ; required = false
      ; doc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
      ; repeated = false
      }
      conv
      ~allow_many
    >>| Option.value ~default
  ;;

  let named_with_default = named_with_default_gen ~allow_many:false

  let named_req ?doc ?value_name ?hidden ?completion names conv =
    named_multi_gen
      { names = names_of_strings names
      ; has_param =
          `Yes_with_value_name (Option.value value_name ~default:conv.default_value_name)
      ; default_string = None
      ; required = true
      ; doc
      ; completion = conv_untyped_completion_opt_with_default conv completion
      ; hidden = Option.value hidden ~default:false
      ; repeated = false
      }
      conv
    |> map' ~f:(function
      | [] ->
        Error
          (Non_ret.Parse_error (Parse_error.Named_req_missing (names_of_strings names)))
      | [ x ] -> Ok x
      | many ->
        Error
          (Non_ret.Parse_error
             (Parse_error.Named_req_appeared_multiple_times
                (names_of_strings names, List.length many))))
  ;;

  let flag_count ?doc ?hidden names =
    let names = names_of_strings names in
    { arg_spec =
        Spec.create_flag
          names
          ~doc
          ~hidden:(Option.value hidden ~default:false)
          ~repeated:true
    ; arg_compute =
        (fun context ->
          Ok (Raw_arg_table.get_flag_count_names context.raw_arg_table names))
    }
  ;;

  let flag_gen ?doc names ~allow_many =
    flag_count ?doc names
    |> map' ~f:(function
      | 0 -> Ok false
      | 1 -> Ok true
      | n ->
        if allow_many
        then Ok true
        else
          Error
            (Non_ret.Parse_error
               (Parse_error.Flag_appeared_multiple_times (names_of_strings names, n))))
  ;;

  let flag = flag_gen ~allow_many:false

  let pos_single_gen i conv ~doc ~value_name ~required ~completion =
    let i =
      match Nonnegative_int.of_int i with
      | Some _ -> i
      | None -> Error.spec_error (Negative_position i)
    in
    { arg_spec =
        Spec.create_positional
          (Spec.Positional.single_at_index
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~required
             ~completion:(conv_untyped_completion_opt_with_default conv completion)
             ~doc)
    ; arg_compute =
        (fun context ->
          match Raw_arg_table.get_pos context.raw_arg_table i with
          | None -> Ok None
          | Some x ->
            (match conv.parse x with
             | Ok x -> Ok (Some x)
             | Error (`Msg message) ->
               Error
                 (Non_ret.Parse_error
                    (Parse_error.Conv_failed { locator = Some (`Positional i); message }))))
    }
  ;;

  let pos_opt ?doc ?value_name ?completion i conv =
    pos_single_gen i conv ~doc ~value_name ~required:false ~completion
  ;;

  let pos_with_default ?doc ?value_name ?completion i conv ~default =
    pos_opt ?doc ?value_name ?completion i conv
    |> map ~f:(function
      | Some x -> x
      | None -> default)
  ;;

  let pos_req ?doc ?value_name ?completion i conv =
    pos_single_gen i conv ~doc ~value_name ~required:true ~completion
    |> map' ~f:(function
      | Some x -> Ok x
      | None -> Error (Non_ret.Parse_error (Parse_error.Pos_req_missing i)))
  ;;

  let pos_left_gen i conv ~doc ~value_name ~required ~completion =
    { arg_spec =
        Spec.create_positional
          (Spec.Positional.all_below_exclusive
             i
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~required
             ~completion:(conv_untyped_completion_opt_with_default conv completion)
             ~doc)
    ; arg_compute =
        (fun context ->
          let left, _ =
            List.split_n (Raw_arg_table.get_pos_all context.raw_arg_table) i
          in
          List.mapi left ~f:(fun i x ->
            Result.map_error (conv.parse x) ~f:(fun (`Msg message) ->
              Non_ret.Parse_error
                (Parse_error.Conv_failed { locator = Some (`Positional i); message })))
          |> Result.List.all)
    }
  ;;

  let pos_left ?doc ?value_name ?completion i conv =
    pos_left_gen i conv ~doc ~value_name ~required:false ~completion
  ;;

  let pos_right_inclusive ?doc ?value_name ?completion i_inclusive conv =
    { arg_spec =
        Spec.create_positional
          (Spec.Positional.all_above_inclusive
             i_inclusive
             ~value_name:(Option.value value_name ~default:conv.default_value_name)
             ~completion:(conv_untyped_completion_opt_with_default conv completion)
             ~doc)
    ; arg_compute =
        (fun context ->
          let _, right =
            List.split_n (Raw_arg_table.get_pos_all context.raw_arg_table) i_inclusive
          in
          List.mapi right ~f:(fun i x ->
            Result.map_error (conv.parse x) ~f:(fun (`Msg message) ->
              Non_ret.Parse_error
                (Parse_error.Conv_failed { locator = Some (`Positional i); message })))
          |> Result.List.all)
    }
  ;;

  let pos_right ?doc ?value_name ?completion i_exclusive conv =
    pos_right_inclusive ?doc ?value_name ?completion (i_exclusive + 1) conv
  ;;

  let pos_all ?doc ?value_name ?completion conv =
    pos_right_inclusive ?doc ?value_name ?completion 0 conv
  ;;

  let validate t = Spec.validate t.arg_spec

  let command_doc_spec
    arg_spec
    (command_line : Command_line.Rich.t)
    ~doc
    ~child_subcommands
    =
    let args = Spec.command_doc_spec arg_spec in
    let subcommands = List.map child_subcommands ~f:Subcommand.command_doc_spec in
    { Command_doc_spec.program_name = command_line.program
    ; subcommand = command_line.subcommand
    ; doc
    ; args
    ; subcommands
    }
  ;;

  let help_spec =
    Spec.create_flag
      Built_in.help_names
      ~doc:(Some "Show this help message.")
      ~hidden:false
      ~repeated:false
  ;;

  let manpage_spec =
    Spec.create_flag Built_in.manpage_names ~doc:None ~hidden:true ~repeated:false
  ;;

  let usage ~doc ~child_subcommands =
    { arg_spec = Spec.empty
    ; arg_compute =
        (fun context ->
          Error
            (Non_ret.Help
               (command_doc_spec help_spec context.command_line ~doc ~child_subcommands)))
    }
  ;;

  let add_help_and_manpage { arg_spec; arg_compute } ~doc ~child_subcommands ~prose =
    let arg_spec = arg_spec |> Spec.merge help_spec |> Spec.merge manpage_spec in
    { arg_spec
    ; arg_compute =
        (fun context ->
          if Raw_arg_table.get_flag_count_names context.raw_arg_table Built_in.help_names
             > 0
          then
            Error
              (Non_ret.Help
                 (command_doc_spec arg_spec context.command_line ~doc ~child_subcommands))
          else if Raw_arg_table.get_flag_count_names
                    context.raw_arg_table
                    Built_in.manpage_names
                  > 0
          then (
            let prose = Option.value prose ~default:Manpage.Prose.empty in
            let spec =
              command_doc_spec arg_spec context.command_line ~doc ~child_subcommands
            in
            Error (Non_ret.Manpage { spec; prose }))
          else arg_compute context)
    }
  ;;

  let finalize t ~doc ~child_subcommands ~prose =
    validate t;
    add_help_and_manpage t ~doc ~child_subcommands ~prose
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
    ; program_exe_for_reentrant_query : [ `Program_name | `Other of string ]
    ; global_symbol_prefix : [ `Random | `Custom of string ]
    ; command_hash_in_function_names : bool
    ; options : Completion.Options.t
    }

  (* An internal argument parser accepting arguments for configuring
     how the completion script is printed *)
  let arg_parser =
    let open Arg_parser in
    let+ program_name =
      named_opt
        ~doc:
          "Name to register this completion script with in the shell. Should be the name \
           of this program's executable. Will default to argv[0]."
        ~value_name:"PROGRAM"
        [ "program-name" ]
        string
    and+ program_exe_for_reentrant_query =
      named_opt
        ~doc:
          "Program to run when executing reentrant queries. This should usually be the \
           same as program-name. Will default to argv[0]. Note that it defaults to \
           argv[0] rather than the value of program-name to help with development \
           workflows, where it's common to manually register a short name as the \
           program-name for testing, but the exe to run is inside a development \
           directory (such as _build)."
        ~value_name:"PROGRAM"
        [ "program-exe-for-reentrant-query" ]
        string
    and+ global_symbol_prefix =
      named_opt
        ~doc:
          "Prefix to use for global symbols in generated completion script. Defaults to \
           \"__climate_complete\" followed by a random int."
        ~value_name:"PREFIX"
        [ "global-symbol-prefix" ]
        string
    and+ no_command_hash_in_function_names =
      flag
        ~doc:
          "Don't add hashes of subcommands to the names of functions that compute \
           suggestions. Hashes are added by default to prevent collisions between \
           generated functions, but such collisions are rare in practice and disabling \
           hashes makes the generated code easier to read."
        [ "no-command-hash-in-function-names" ]
    and+ no_comments =
      flag ~doc:"Omit comments from the generated completion script." [ "no-comments" ]
    and+ no_whitespace =
      flag
        ~doc:"Remove unnecessary whitespace from generated completion script."
        [ "no-whitespace" ]
    and+ minify_global_names =
      flag
        ~doc:
          "Rename global variables and functions in completion script to be as short as \
           possible."
        [ "minify-global-names" ]
    and+ minify_local_variables =
      flag
        ~doc:"Use short names for local variables in generated bash script."
        [ "minify-local-variables" ]
    and+ optimize_case_statements =
      flag
        ~doc:
          "Combine sequences of contiguous case bodies in cases statements, merging \
           their patterns."
        [ "optimize-case-statements" ]
    in
    let program_name =
      match program_name with
      | Some program_name -> program_name
      | None -> Sys.argv.(0)
    in
    let program_exe_for_reentrant_query =
      match program_exe_for_reentrant_query with
      | Some program_exe_for_reentrant_query -> `Other program_exe_for_reentrant_query
      | None -> `Other Sys.argv.(0)
    in
    let global_symbol_prefix =
      match global_symbol_prefix with
      | Some global_symbol_prefix -> `Custom global_symbol_prefix
      | None -> `Random
    in
    let options =
      { Completion_.Options.no_comments
      ; no_whitespace
      ; minify_global_names
      ; minify_local_variables
      ; optimize_case_statements
      }
    in
    { program_name
    ; program_exe_for_reentrant_query
    ; global_symbol_prefix
    ; command_hash_in_function_names = not no_command_hash_in_function_names
    ; options
    }
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

module Program_name = struct
  type t =
    | Argv0
    | Literal of string

  let get = function
    | Argv0 -> Sys.argv.(0)
    | Literal name -> name
  ;;
end

module Command = struct
  type internal = Print_completion_script_bash

  let internal_doc = function
    | Print_completion_script_bash -> "Print the bash completion script for this program."
  ;;

  let internal_arg_spec = function
    | Print_completion_script_bash -> Completion_config.arg_parser.arg_spec
  ;;

  module Subcommand_info = struct
    type t =
      { name : Name.t
      ; aliases : Name.t list
      ; hidden : bool
      }

    let matches { name; aliases; _ } s =
      let string_matches_name name = String.equal s (Name.to_string name) in
      string_matches_name name || List.exists aliases ~f:string_matches_name
    ;;
  end

  type 'a t =
    | Singleton of
        { arg_parser : 'a Arg_parser.t
        ; doc : string option
        }
    | Group of
        { children : 'a subcommand list
        ; default_arg_parser : 'a Arg_parser.t
        ; doc : string option
        }
    | Internal of internal

  and 'a subcommand =
    { info : Subcommand_info.t
    ; command : 'a t
    }

  let command_doc = function
    | Singleton { doc; _ } | Group { doc; _ } -> doc
    | Internal internal -> Some (internal_doc internal)
  ;;

  let command_arg_spec = function
    | Singleton { arg_parser; _ } -> arg_parser.arg_spec
    | Group { default_arg_parser; _ } -> default_arg_parser.arg_spec
    | Internal internal -> internal_arg_spec internal
  ;;

  let singleton ?doc ?prose arg_parser =
    let doc = doc in
    Singleton
      { arg_parser = Arg_parser.finalize arg_parser ~doc ~child_subcommands:[] ~prose
      ; doc
      }
  ;;

  let subcommand ?(hidden = false) ?(aliases = []) name_string command =
    let name = name_of_string_exn name_string in
    let aliases = List.map aliases ~f:name_of_string_exn in
    { info = { Subcommand_info.name; hidden; aliases }; command }
  ;;

  let group ?default_arg_parser ?doc ?prose children =
    let child_subcommands =
      List.filter_map children ~f:(fun { info; command } ->
        if info.hidden
        then None
        else
          Some
            { Subcommand.name = info.name
            ; aliases = info.aliases
            ; doc = command_doc command
            ; arg_spec = command_arg_spec command
            })
    in
    let default_arg_parser =
      match default_arg_parser with
      | None -> Arg_parser.usage ~doc ~child_subcommands
      | Some default_arg_parser -> default_arg_parser
    in
    let default_arg_parser =
      Arg_parser.finalize default_arg_parser ~doc ~child_subcommands ~prose
    in
    let () =
      match
        List.concat_map children ~f:(fun { info; _ } ->
          List.map ~f:Name.to_string (info.name :: info.aliases))
        |> String.find_duplicates
      with
      | [] -> ()
      | child_names ->
        Error.spec_error (Error.Spec_error.Duplicate_command_names child_names)
    in
    Group { children; default_arg_parser; doc }
  ;;

  let print_completion_script_bash = Internal Print_completion_script_bash

  type 'a traverse =
    { operation : [ `Arg_parser of 'a Arg_parser.t | `Internal of internal ]
    ; args : string list
    ; subcommand : string list
    }

  let rec traverse t args subcommand_acc =
    match t, args with
    | Singleton { arg_parser; doc = _ }, args ->
      { operation = `Arg_parser arg_parser; args; subcommand = List.rev subcommand_acc }
    | Group { children; default_arg_parser; doc = _ }, x :: xs ->
      let subcommand =
        List.find_map children ~f:(fun { info; command } ->
          if Subcommand_info.matches info x then Some (command, info.name) else None)
      in
      (match subcommand with
       | Some (subcommand, name) ->
         traverse subcommand xs (Name.to_string name :: subcommand_acc)
       | None ->
         { operation = `Arg_parser default_arg_parser
         ; args = x :: xs
         ; subcommand = List.rev subcommand_acc
         })
    | Group { children = _; default_arg_parser; doc = _ }, [] ->
      { operation = `Arg_parser default_arg_parser
      ; args = []
      ; subcommand = List.rev subcommand_acc
      }
    | Internal internal, args ->
      { operation = `Internal internal; args; subcommand = List.rev subcommand_acc }
  ;;

  let rec completion_spec = function
    | Singleton { arg_parser; doc = _ } ->
      let parser_spec = Spec.to_completion_parser_spec arg_parser.arg_spec in
      { Completion_spec.parser_spec; subcommands = [] }
    | Internal Print_completion_script_bash ->
      let parser_spec =
        Spec.to_completion_parser_spec Completion_config.arg_parser.arg_spec
      in
      { Completion_spec.parser_spec; subcommands = [] }
    | Group { children; default_arg_parser; doc = _ } ->
      let parser_spec = Spec.to_completion_parser_spec default_arg_parser.arg_spec in
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
    ?(program_exe_for_reentrant_query = `Program_name)
    ?(global_symbol_prefix = `Random)
    ?(command_hash_in_function_names = true)
    ?(program_name = Program_name.Argv0)
    ?(options = Completion.Options.default)
    t
    =
    completion_spec t
    |> Completion.generate_bash
         ~print_reentrant_completions_name:eval_config.print_reentrant_completions_name
         ~program_name:(Program_name.get program_name)
         ~program_exe_for_reentrant_query
         ~global_symbol_prefix
         ~command_hash_in_function_names
         ~options
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
      | [] -> Ok None
      | _ ->
        let command_line =
          { Command_line.Rich.program = raw_command_line.program
          ; args = raw_command_line.args
          ; subcommand = []
          }
        in
        Arg_parser.eval (arg_parser name) ~command_line ~ignore_errors:true
    ;;

    let run_query
      t
      command
      (completion_spec : Spec.untyped_completion_function Completion_spec.t)
      =
      let all_reentrants = Completion_spec.all_reentrants completion_spec in
      match List.nth_opt all_reentrants t.index with
      | Some reentrant ->
        let { subcommand; args; _ } = traverse command t.command_line.args [] in
        reentrant { Command_line.Rich.program = t.command_line.program; subcommand; args }
      | None ->
        failwith
          "reentrant query was invoked with an out of bounds argument, which the \
           completion script should never do"
    ;;
  end

  let eval_internal
    (eval_config : Eval_config.t)
    t
    (raw_command_line : Command_line.Raw.t)
    =
    let open Result.O in
    let completion_spec = completion_spec t in
    (* If the top-level command was passed the reentrant query
       argument, cancel normal operation and just invoke the appropriate
       reentrant function. *)
    let* () =
      Reentrant_query.eval_arg_parser
        eval_config.print_reentrant_completions_name
        raw_command_line
      >>= function
      | None -> Ok ()
      | Some reentrant_query ->
        let suggestions = Reentrant_query.run_query reentrant_query t completion_spec in
        Error (Non_ret.Reentrant_query { suggestions })
    in
    let { operation; args; subcommand } = traverse t raw_command_line.args [] in
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
      let arg_parser =
        Arg_parser.finalize
          Completion_config.arg_parser
          ~doc:(Some (internal_doc Print_completion_script_bash))
          ~child_subcommands:[]
          ~prose:None
      in
      (* Print the completion script. Note that this can't be combined
         into the regular parser logic because it needs to know the
         completion spec, which isn't available to regular argument
         parsers. *)
      let* { Completion_config.program_name
           ; program_exe_for_reentrant_query
           ; global_symbol_prefix
           ; command_hash_in_function_names
           ; options
           }
        =
        Arg_parser.eval arg_parser ~command_line ~ignore_errors:false
      in
      Error
        (Non_ret.Generate_completion_script
           { completion_script =
               Completion.generate_bash
                 completion_spec
                 ~program_name
                 ~program_exe_for_reentrant_query
                 ~print_reentrant_completions_name:
                   eval_config.print_reentrant_completions_name
                 ~global_symbol_prefix
                 ~command_hash_in_function_names
                 ~options
           })
  ;;

  (* All the side effects that can be requested by a parser. Does not return.
     [test_friendly] prevents this function from exiting the program and prints
     all of its output to stdout. *)
  let handle_non_ret non_ret ~help_style ~version ~test_friendly =
    let () =
      match (non_ret : Non_ret.t) with
      | Help spec -> Help.pp help_style Format.std_formatter spec
      | Manpage { spec; prose } ->
        let manpage = { Manpage.spec; prose; version } in
        print_endline (Manpage.to_troff_string manpage)
      | Reentrant_query { suggestions } -> List.iter suggestions ~f:print_endline
      | Parse_error parse_error ->
        if test_friendly
        then print_endline (Parse_error.to_string parse_error)
        else (
          Printf.eprintf "%s" (Parse_error.to_string parse_error);
          exit Parse_error.exit_code)
      | Generate_completion_script { completion_script } ->
        print_endline completion_script
    in
    exit 0
  ;;

  let handle_result result ~help_style ~version =
    match result with
    | Ok x -> x
    | Error non_ret -> handle_non_ret non_ret ~help_style ~version ~test_friendly:false
  ;;

  let run
    ?(eval_config = Eval_config.default)
    ?(program_name = Program_name.Argv0)
    ?(help_style = Help_style.default)
    ?version
    t
    =
    let command_line_program_name_is_argv0 = Command_line.Raw.from_env () in
    let command_line =
      match (program_name : Program_name.t) with
      | Argv0 -> command_line_program_name_is_argv0
      | Literal program -> { command_line_program_name_is_argv0 with program }
    in
    eval_internal eval_config t command_line |> handle_result ~help_style ~version
  ;;

  let run_singleton
    ?(eval_config = Eval_config.default)
    ?(program_name = Program_name.Argv0)
    ?(help_style = Help_style.default)
    ?version
    ?doc
    arg_parser
    =
    run ~eval_config ~program_name ~help_style ?version (singleton ?doc arg_parser)
  ;;

  let eval
    ?(eval_config = Eval_config.default)
    ?(program_name = Program_name.Argv0)
    ?(help_style = Help_style.default)
    ?version
    t
    args
    =
    eval_internal
      eval_config
      t
      { Command_line.Raw.args; program = Program_name.get program_name }
    |> handle_result ~help_style ~version
  ;;
end

module For_test = struct
  module Climate_stdlib = Climate_stdlib
  module Non_ret = Non_ret
  module Parse_error = Error.Parse_error

  let eval_result ~program_name t args =
    Command.eval_internal
      Eval_config.default
      t
      { Command_line.Raw.args; program = program_name }
  ;;

  let print_help_spec spec = Help.pp Help_style.plain Format.std_formatter spec

  let print_manpage spec prose =
    let manpage = { Manpage.spec; prose; version = None } in
    print_endline (Manpage.to_troff_string manpage)
  ;;
end
