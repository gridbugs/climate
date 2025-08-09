open! Import
module Completion_ = Completion
module Parse_error = Error.Parse_error
module Spec_error = Error.Spec_error

let name_of_string_exn string =
  match Name.of_string string with
  | Ok name -> name
  | Error e -> Error.spec_error (Invalid_name (string, e))
;;

module Command_doc = struct
  (* Documentation about an entire (sub)command. Not relevant to parsing
     arguments but needs to be included in some error messages. *)
  type t =
    { doc : string option
    ; child_subcommands : Subcommand.t list
    }

  let empty = { doc = None; child_subcommands = [] }
end

module Context = struct
  type t =
    { raw_arg_table : Raw_arg_table.t
    ; command_line : Command_line.Rich.t
    ; command_doc_spec : Command_doc_spec.t
    ; error_subcommand : string list
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
  ; command_doc : Command_doc.t
  }

(* Create a parser with no command documentation. This is the standard way of
   creating a parser, as conceptually we often don't need to think of a
   parser as even being part of a command. The main exception to this is when
   returning a [Non_ret.t], as these must contain a specification of the
   current command for printing usage strings in response to (say) an error
   parsing command line arguments. The [command_doc] field is set when adding
   the --help argument, and it will be added to the context passed to
   [arg_compute] so argument parsing logic has access to the final command
   documentation for use in error messages. *)
let with_empty_command_doc ~arg_spec ~arg_compute =
  { arg_spec; arg_compute; command_doc = Command_doc.empty }
;;

let spec { arg_spec; _ } = arg_spec

let command_doc_spec
  arg_spec
  (command_doc : Command_doc.t)
  (command_line : Command_line.Rich.t)
  =
  let args = Spec.command_doc_spec arg_spec in
  let subcommands =
    List.map command_doc.child_subcommands ~f:Subcommand.command_doc_spec
  in
  { Command_doc_spec.program_name = command_line.program
  ; subcommand = command_line.subcommand
  ; doc = command_doc.doc
  ; args
  ; subcommands
  }
;;

let eval
  t
  ~(command_line : Command_line.Rich.t)
  ~ignore_errors
  ~alt_subcommand_for_usage
  ~alt_subcommand_for_errors
  =
  let open Result.O in
  let subcommand_for_errors =
    Option.value alt_subcommand_for_errors ~default:command_line.subcommand
  in
  let subcommand_for_usage =
    Option.value alt_subcommand_for_usage ~default:command_line.subcommand
  in
  let command_doc_spec subcommand =
    { (command_doc_spec t.arg_spec t.command_doc command_line) with subcommand }
  in
  let* raw_arg_table =
    Raw_arg_table.parse t.arg_spec command_line.args ~ignore_errors
    |> Result.map_error ~f:(fun error ->
      Non_ret.Parse_error
        { command_doc_spec = command_doc_spec subcommand_for_errors; error })
  in
  let context =
    { Context.raw_arg_table
    ; command_line
    ; command_doc_spec = command_doc_spec subcommand_for_usage
    ; error_subcommand = subcommand_for_errors
    }
  in
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
      match
        eval
          parser
          ~command_line
          ~ignore_errors:true
          ~alt_subcommand_for_errors:None
          ~alt_subcommand_for_usage:None
      with
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

let file = { string with default_value_name = "FILE"; completion = Some Completion.file }

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

let map { arg_spec; arg_compute; command_doc } ~f =
  { arg_spec
  ; arg_compute = (fun context -> Result.map ~f (arg_compute context))
  ; command_doc
  }
;;

let map' { arg_spec; arg_compute; command_doc } ~f =
  { arg_spec
  ; arg_compute = (fun context -> Result.bind ~f (arg_compute context))
  ; command_doc
  }
;;

let map_context' { arg_spec; arg_compute; command_doc } ~f =
  { arg_spec
  ; arg_compute =
      (fun context -> Result.bind ~f:(fun x -> f x context) (arg_compute context))
  ; command_doc
  }
;;

let both x y =
  { arg_spec = Spec.merge x.arg_spec y.arg_spec
  ; arg_compute =
      (fun context ->
        let open Result.O in
        let+ x_value = x.arg_compute context
        and+ y_value = y.arg_compute context in
        x_value, y_value)
  ; command_doc =
      (* It's not expected that args will be both'd once their command
         documentation has been added. *)
      y.command_doc
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

let const x =
  with_empty_command_doc ~arg_spec:Spec.empty ~arg_compute:(fun _context -> Ok x)
;;

let unit = const ()

let argv0 =
  with_empty_command_doc ~arg_spec:Spec.empty ~arg_compute:(fun context ->
    Ok context.command_line.program)
;;

let last { arg_spec; arg_compute; command_doc } =
  { arg_spec
  ; command_doc
  ; arg_compute =
      (fun context ->
        Result.bind (arg_compute context) ~f:(fun list ->
          match List.last list with
          | None ->
            Error
              (Non_ret.Parse_error
                 { error =
                     Conv_failed { locator = None; message = "Unexpected empty list" }
                 ; command_doc_spec = context.command_doc_spec
                 })
          | Some x -> Ok x))
  }
;;

let named_multi_gen info conv =
  with_empty_command_doc ~arg_spec:(Spec.create_named info) ~arg_compute:(fun context ->
    Raw_arg_table.get_opts_names_by_name context.raw_arg_table info.names
    |> List.map ~f:(fun (name, value) ->
      conv.parse value
      |> Result.map_error ~f:(fun (`Msg message) ->
        Non_ret.Parse_error
          { error = Conv_failed { locator = Some (`Named name); message }
          ; command_doc_spec = context.command_doc_spec
          }))
    |> Result.List.all)
;;

let named_opt_gen (info : Spec.Named.Info.t) conv ~allow_many =
  named_multi_gen info conv
  |> map_context' ~f:(fun xs context ->
    match xs with
    | [] -> Ok None
    | [ x ] -> Ok (Some x)
    | x :: _ as many ->
      if allow_many
      then Ok (Some x)
      else
        Error
          (Non_ret.Parse_error
             { error = Named_opt_appeared_multiple_times (info.names, List.length many)
             ; command_doc_spec = context.command_doc_spec
             }))
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
  |> map_context' ~f:(fun xs context ->
    match xs with
    | [] ->
      Error
        (Non_ret.Parse_error
           { error = Named_req_missing (names_of_strings names)
           ; command_doc_spec = context.command_doc_spec
           })
    | [ x ] -> Ok x
    | many ->
      Error
        (Non_ret.Parse_error
           { error =
               Named_req_appeared_multiple_times (names_of_strings names, List.length many)
           ; command_doc_spec = context.command_doc_spec
           }))
;;

let flag_count ?doc ?hidden names =
  let names = names_of_strings names in
  with_empty_command_doc
    ~arg_spec:
      (Spec.create_flag
         names
         ~doc
         ~hidden:(Option.value hidden ~default:false)
         ~repeated:true)
    ~arg_compute:(fun context ->
      Ok (Raw_arg_table.get_flag_count_names context.raw_arg_table names))
;;

let flag_gen ?doc names ~allow_many =
  flag_count ?doc names
  |> map_context' ~f:(fun n context ->
    match n with
    | 0 -> Ok false
    | 1 -> Ok true
    | n ->
      if allow_many
      then Ok true
      else
        Error
          (Non_ret.Parse_error
             { error = Flag_appeared_multiple_times (names_of_strings names, n)
             ; command_doc_spec = context.command_doc_spec
             }))
;;

let flag = flag_gen ~allow_many:false

let pos_single_gen i conv ~doc ~value_name ~required ~completion =
  let i =
    match Nonnegative_int.of_int i with
    | Some _ -> i
    | None -> Error.spec_error (Negative_position i)
  in
  with_empty_command_doc
    ~arg_spec:
      (Spec.create_positional
         (Spec.Positional.single_at_index
            i
            ~value_name:(Option.value value_name ~default:conv.default_value_name)
            ~required
            ~completion:(conv_untyped_completion_opt_with_default conv completion)
            ~doc))
    ~arg_compute:(fun context ->
      match Raw_arg_table.get_pos context.raw_arg_table i with
      | None -> Ok None
      | Some x ->
        (match conv.parse x with
         | Ok x -> Ok (Some x)
         | Error (`Msg message) ->
           Error
             (Non_ret.Parse_error
                { error = Conv_failed { locator = Some (`Positional i); message }
                ; command_doc_spec = context.command_doc_spec
                })))
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
  |> map_context' ~f:(fun x context ->
    match x with
    | Some x -> Ok x
    | None ->
      Error
        (Non_ret.Parse_error
           { error = Pos_req_missing i; command_doc_spec = context.command_doc_spec }))
;;

let pos_left_gen i conv ~doc ~value_name ~required ~completion =
  with_empty_command_doc
    ~arg_spec:
      (Spec.create_positional
         (Spec.Positional.all_below_exclusive
            i
            ~value_name:(Option.value value_name ~default:conv.default_value_name)
            ~required
            ~completion:(conv_untyped_completion_opt_with_default conv completion)
            ~doc))
    ~arg_compute:(fun context ->
      let left, _ = List.split_n (Raw_arg_table.get_pos_all context.raw_arg_table) i in
      List.mapi left ~f:(fun i x ->
        Result.map_error (conv.parse x) ~f:(fun (`Msg message) ->
          Non_ret.Parse_error
            { error = Conv_failed { locator = Some (`Positional i); message }
            ; command_doc_spec = context.command_doc_spec
            }))
      |> Result.List.all)
;;

let pos_left ?doc ?value_name ?completion i conv =
  pos_left_gen i conv ~doc ~value_name ~required:false ~completion
;;

let pos_right_inclusive ?doc ?value_name ?completion i_inclusive conv =
  with_empty_command_doc
    ~arg_spec:
      (Spec.create_positional
         (Spec.Positional.all_above_inclusive
            i_inclusive
            ~value_name:(Option.value value_name ~default:conv.default_value_name)
            ~completion:(conv_untyped_completion_opt_with_default conv completion)
            ~doc))
    ~arg_compute:(fun context ->
      let _, right =
        List.split_n (Raw_arg_table.get_pos_all context.raw_arg_table) i_inclusive
      in
      List.mapi right ~f:(fun i x ->
        Result.map_error (conv.parse x) ~f:(fun (`Msg message) ->
          Non_ret.Parse_error
            { error = Conv_failed { locator = Some (`Positional i); message }
            ; command_doc_spec = context.command_doc_spec
            }))
      |> Result.List.all)
;;

let pos_right ?doc ?value_name ?completion i_exclusive conv =
  pos_right_inclusive ?doc ?value_name ?completion (i_exclusive + 1) conv
;;

let pos_all ?doc ?value_name ?completion conv =
  pos_right_inclusive ?doc ?value_name ?completion 0 conv
;;

let validate t = Spec.validate t.arg_spec

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

let usage ~error ~message ~override_doc =
  with_empty_command_doc ~arg_spec:Spec.empty ~arg_compute:(fun context ->
    let doc =
      match override_doc with
      | Some override_doc -> Some override_doc
      | None -> context.command_doc_spec.doc
    in
    Error
      (Non_ret.Help
         { command_doc_spec = { context.command_doc_spec with doc }; error; message }))
;;

let to_usage { arg_spec; arg_compute = _; command_doc } =
  { arg_spec = Spec.empty
  ; command_doc = Command_doc.empty
  ; arg_compute =
      (fun context ->
        Error
          (Non_ret.Help
             { command_doc_spec =
                 { context.command_doc_spec with
                   args = Spec.command_doc_spec arg_spec
                 ; doc = command_doc.doc
                 }
             ; error = false
             ; message = None
             }))
  }
;;

let add_help_and_manpage
  { arg_spec; arg_compute; command_doc = _ }
  ~doc
  ~child_subcommands
  ~prose
  ~use_error_subcommand
  ~help_only_doc
  ~help_only_subcommands
  =
  let command_doc = { Command_doc.doc; child_subcommands } in
  let arg_spec = arg_spec |> Spec.merge help_spec |> Spec.merge manpage_spec in
  { arg_spec
  ; arg_compute =
      (fun context ->
        if Raw_arg_table.get_flag_count_names context.raw_arg_table Built_in.help_names
           > 0
        then (
          let command_doc_spec =
            match use_error_subcommand with
            | false -> context.command_doc_spec
            | true ->
              { context.command_doc_spec with subcommand = context.error_subcommand }
          in
          let command_doc_spec =
            match help_only_doc with
            | None -> command_doc_spec
            | Some doc -> { command_doc_spec with doc = Some doc }
          in
          let command_doc_spec =
            match help_only_subcommands with
            | None -> command_doc_spec
            | Some help_only_subcommands ->
              let help_only_subcommands =
                List.map help_only_subcommands ~f:Subcommand.command_doc_spec
              in
              { command_doc_spec with subcommands = help_only_subcommands }
          in
          Error (Non_ret.Help { command_doc_spec; error = false; message = None }))
        else if Raw_arg_table.get_flag_count_names
                  context.raw_arg_table
                  Built_in.manpage_names
                > 0
        then (
          let prose = Option.value prose ~default:Manpage.Prose.empty in
          Error (Non_ret.Manpage { prose; command_doc_spec = context.command_doc_spec }))
        else arg_compute context)
  ; command_doc
  }
;;

let finalize
  t
  ~doc
  ~child_subcommands
  ~prose
  ~use_error_subcommand
  ~help_only_doc
  ~help_only_subcommands
  =
  validate t;
  add_help_and_manpage
    t
    ~doc
    ~child_subcommands
    ~prose
    ~use_error_subcommand
    ~help_only_doc
    ~help_only_subcommands
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

module Private = struct
  let usage = usage
  let to_usage = to_usage
  let spec = spec
  let finalize = finalize
  let named_opt_for_internal = named_opt_for_internal
  let eval = eval

  let command_doc_spec { arg_spec; command_doc; _ } command_line =
    command_doc_spec arg_spec command_doc command_line
  ;;
end
