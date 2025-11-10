open! Import
module Parse_error = Error.Parse_error
module Spec_error = Error.Spec_error
module Arg_parser = Arg_parser

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
    let module Completion_ = Completion in
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

let help_command_arg_parser () =
  Arg_parser.Private.usage ~error:false ~message:None ~override_doc:None
;;

module Completion_options = Completion.Options

module Command = struct
  type internal =
    | Print_completion_script_bash
    | Help

  let internal_doc = function
    | Print_completion_script_bash -> "Print the bash completion script for this program."
    | Help -> "Print documentation for a subcommand."
  ;;

  let internal_arg_spec = function
    | Print_completion_script_bash -> Arg_parser.Private.spec Completion_config.arg_parser
    | Help -> Arg_parser.Private.spec (help_command_arg_parser ())
  ;;

  let finalized_print_completion_script_bash_arg_parser =
    Arg_parser.Private.finalize
      Completion_config.arg_parser
      ~doc:(Some (internal_doc Print_completion_script_bash))
      ~child_subcommands:[]
      ~prose:None
      ~use_error_subcommand:false
      ~help_only_doc:None
      ~help_only_subcommands:None
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
    ; help : bool (* This subcommand is a help subcommand *)
    }

  let command_doc = function
    | Singleton { doc; _ } | Group { doc; _ } -> doc
    | Internal internal -> Some (internal_doc internal)
  ;;

  let command_arg_spec = function
    | Singleton { arg_parser; _ } -> Arg_parser.Private.spec arg_parser
    | Group { default_arg_parser; _ } -> Arg_parser.Private.spec default_arg_parser
    | Internal internal -> internal_arg_spec internal
  ;;

  let singleton ?doc ?prose arg_parser =
    let doc = doc in
    Singleton
      { arg_parser =
          Arg_parser.Private.finalize
            arg_parser
            ~doc
            ~child_subcommands:[]
            ~prose
            ~use_error_subcommand:false
            ~help_only_doc:None
            ~help_only_subcommands:None
      ; doc
      }
  ;;

  let subcommand ?(hidden = false) ?(aliases = []) name_string command =
    let name = name_of_string_exn name_string in
    let aliases = List.map aliases ~f:name_of_string_exn in
    { info = { Subcommand_info.name; hidden; aliases }; command; help = false }
  ;;

  let expand_subcommand_info subcommands =
    List.filter_map subcommands ~f:(fun { info; command; help = _ } ->
      if info.hidden
      then None
      else
        Some
          { Subcommand.name = info.name
          ; aliases = info.aliases
          ; doc = command_doc command
          ; arg_spec = command_arg_spec command
          })
  ;;

  let group' ~default_arg_parser ~doc ~prose children =
    let child_subcommands = expand_subcommand_info children in
    let finalize parser_ =
      Arg_parser.Private.finalize
        parser_
        ~doc
        ~child_subcommands
        ~prose
        ~use_error_subcommand:false
        ~help_only_doc:None
        ~help_only_subcommands:None
    in
    let default_arg_parser =
      match default_arg_parser with
      | `None ->
        (* By default, use an arg parser that just prints a usage message. *)
        Arg_parser.Private.usage
          ~error:true
          ~message:(Some "This command requires a subcommand but none was specified.")
          ~override_doc:None
        |> finalize
      | `Not_finalized default_arg_parser -> finalize default_arg_parser
      | `Finalized default_arg_parser -> default_arg_parser
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

  let group ?default_arg_parser ?doc ?prose children =
    let default_arg_parser =
      match default_arg_parser with
      | None -> `None
      | Some default_arg_parser -> `Not_finalized default_arg_parser
    in
    group' ~default_arg_parser ~doc ~prose children
  ;;

  (* Recursively replace all parsers in t with parsers which just print their
     usage strings rather than having any side-effects. Used to implement help
     commands. The provided [finalize] function makes it so that the help
     messages for the replaced parsers references the original help command's
     subcommands rather than whichever command was run with "--help". *)
  let rec replace_with_usage t ~finalize =
    match t with
    | Singleton { arg_parser; doc } ->
      let arg_parser =
        Arg_parser.Private.to_usage arg_parser |> finalize ~child_subcommands:[]
      in
      Singleton { arg_parser; doc }
    | Internal Print_completion_script_bash ->
      let arg_parser =
        Arg_parser.Private.to_usage Completion_config.arg_parser
        |> finalize ~child_subcommands:[]
      in
      Singleton { arg_parser; doc = Some (internal_doc Print_completion_script_bash) }
    | Internal Help ->
      let arg_parser =
        Arg_parser.Private.usage
          ~error:false
          ~message:None
          ~override_doc:(Some "Subcommand help. Pass the name of a subcommand for info.")
        |> finalize ~child_subcommands:[]
      in
      Singleton { arg_parser; doc = Some (internal_doc Help) }
    | Group { children; default_arg_parser; doc } ->
      let child_subcommands = expand_subcommand_info children in
      let default_arg_parser =
        Arg_parser.Private.to_usage default_arg_parser |> finalize ~child_subcommands
      in
      let children =
        List.map children ~f:(fun { info; command; help } ->
          let command = replace_with_usage command ~finalize in
          { info; command; help })
      in
      Group { children; default_arg_parser; doc }
  ;;

  (* Replace all occurences of [Internal Help] with a command group of doc
     commands matching the layout of its sibling subcommands. *)
  let preprocess_help t =
    let rec loop ~siblings ~doc t =
      match t with
      | Singleton _ | Internal Print_completion_script_bash -> t
      | Group { children; default_arg_parser; doc } ->
        let children =
          List.map children ~f:(fun { info; command; help } ->
            let help =
              match command with
              | Internal Help -> true
              | _ -> help
            in
            { info; command = loop ~siblings:children command ~doc; help })
        in
        Group { children; default_arg_parser; doc }
      | Internal Help ->
        let help_only_subcommands = expand_subcommand_info siblings in
        let finalize ~child_subcommands command =
          Arg_parser.Private.finalize
            ~doc
            ~child_subcommands
            ~prose:None
            ~use_error_subcommand:true
            ~help_only_doc:
              (Some "Subcommand help. Pass the name of a subcommand (see below) for info.")
            ~help_only_subcommands:(Some help_only_subcommands)
            command
        in
        let siblings =
          List.map siblings ~f:(fun { info; command; help } ->
            { info; command = replace_with_usage command ~finalize; help })
        in
        let default_arg_parser =
          `Finalized
            (help_command_arg_parser ()
             |> finalize ~child_subcommands:help_only_subcommands)
        in
        group' ~doc ~default_arg_parser ~prose:None siblings
    in
    loop ~siblings:[] ~doc:None t
  ;;

  let print_completion_script_bash = Internal Print_completion_script_bash
  let help = Internal Help

  type traverse_help =
    { subcommand_up_to_help : string list
    ; subcommand_excluding_help : string list
    }

  type 'a traverse =
    { operation : [ `Arg_parser of 'a Arg_parser.t | `Print_completion_script_bash ]
    ; args : string list
    ; subcommand : string list
    ; raw_command_line : Command_line.Raw.t
    ; traverse_help : traverse_help option
    }

  let traverse_to_rich_command_line { args; subcommand; raw_command_line; _ } =
    { Command_line.Rich.program = raw_command_line.program; args; subcommand }
  ;;

  let traverse t (raw_command_line : Command_line.Raw.t) =
    let rec loop t args subcommand_acc ~traverse_help =
      match t, args with
      | Singleton { arg_parser; doc = _ }, args ->
        Ok
          { operation = `Arg_parser arg_parser
          ; args
          ; subcommand = List.rev subcommand_acc
          ; raw_command_line
          ; traverse_help
          }
      | Group { children; default_arg_parser; doc = _ }, x :: xs ->
        let subcommand =
          List.find_map children ~f:(fun { info; command; help } ->
            if Subcommand_info.matches info x
            then Some (command, info.name, help)
            else None)
        in
        (match subcommand with
         | Some (subcommand, name, this_subcommand_is_help) ->
           let subcommand_acc_including_current = Name.to_string name :: subcommand_acc in
           let traverse_help =
             match traverse_help, this_subcommand_is_help with
             | Some { subcommand_up_to_help; subcommand_excluding_help }, _ ->
               Some
                 { subcommand_up_to_help
                 ; subcommand_excluding_help =
                     Name.to_string name :: subcommand_excluding_help
                 }
             | None, false -> None
             | None, true ->
               Some
                 { subcommand_up_to_help = subcommand_acc_including_current
                 ; subcommand_excluding_help = subcommand_acc
                 }
           in
           loop subcommand xs subcommand_acc_including_current ~traverse_help
         | None ->
           (* The current word doesn't correspond to a subcommand, however it's
              possible that it's intended as an argument for the default arg
              parser of the current command. *)
           if (not (String.starts_with x ~prefix:"-"))
              && Spec.Positional.is_empty
                   (Arg_parser.Private.spec default_arg_parser).positional
           then (
             (* A positional argument was passed when the default arg parser
                doesn't accept any positional arguments, so it's very likely that
                the user mistyped the name of a subcommand instead. Throw a parse
                error to that affect. *)
             let rich_command_line =
               { Command_line.Rich.program = raw_command_line.program
               ; args = raw_command_line.args
               ; subcommand = subcommand_acc
               }
             in
             let command_doc_spec =
               Arg_parser.Private.command_doc_spec default_arg_parser rich_command_line
             in
             Error
               (Non_ret.Parse_error { error = No_such_subcommand x; command_doc_spec }))
           else
             Ok
               { operation = `Arg_parser default_arg_parser
               ; args = x :: xs
               ; subcommand = List.rev subcommand_acc
               ; raw_command_line
               ; traverse_help
               })
      | Group { children = _; default_arg_parser; doc = _ }, [] ->
        Ok
          { operation = `Arg_parser default_arg_parser
          ; args = []
          ; subcommand = List.rev subcommand_acc
          ; raw_command_line
          ; traverse_help
          }
      | Internal Print_completion_script_bash, args ->
        Ok
          { operation = `Print_completion_script_bash
          ; args
          ; subcommand = List.rev subcommand_acc
          ; raw_command_line
          ; traverse_help
          }
      | Internal Help, _args ->
        failwith
          "All help commands should have been replaced by this point. This is a bug in \
           climate."
    in
    loop t raw_command_line.args [] ~traverse_help:None
    |> Result.map ~f:(fun ret ->
      (* If there are traverse_help fields their contents will be backwards
         at this point, so reverse them. *)
      { ret with
        traverse_help =
          Option.map
            ret.traverse_help
            ~f:(fun { subcommand_up_to_help; subcommand_excluding_help } ->
              { subcommand_up_to_help = List.rev subcommand_up_to_help
              ; subcommand_excluding_help = List.rev subcommand_excluding_help
              })
      })
  ;;

  let rec completion_spec = function
    | Singleton { arg_parser; doc = _ } ->
      let parser_spec =
        Spec.to_completion_parser_spec (Arg_parser.Private.spec arg_parser)
      in
      { Completion_spec.parser_spec; subcommands = [] }
    | Internal Print_completion_script_bash ->
      let parser_spec =
        Spec.to_completion_parser_spec
          (Arg_parser.Private.spec Completion_config.arg_parser)
      in
      { Completion_spec.parser_spec; subcommands = [] }
    | Internal Help ->
      let parser_spec =
        Spec.to_completion_parser_spec
          (Arg_parser.Private.spec (help_command_arg_parser ()))
      in
      { Completion_spec.parser_spec; subcommands = [] }
    | Group { children; default_arg_parser; doc = _ } ->
      let parser_spec =
        Spec.to_completion_parser_spec (Arg_parser.Private.spec default_arg_parser)
      in
      let subcommands =
        List.filter_map children ~f:(fun { info; command; help = _ } ->
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
      let+ index = Private.named_opt_for_internal [ name ] int ~allow_many:true
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
        Arg_parser.Private.eval
          (arg_parser name)
          ~command_line
          ~ignore_errors:true
          ~alt_subcommand_for_usage:None
          ~alt_subcommand_for_errors:None
    ;;

    let run_query
      t
      command
      (completion_spec : Spec.untyped_completion_function Completion_spec.t)
      =
      let all_reentrants = Completion_spec.all_reentrants completion_spec in
      match List.nth_opt all_reentrants t.index with
      | Some reentrant ->
        (match traverse command t.command_line with
         | Ok { subcommand; args; _ } ->
           reentrant
             { Command_line.Rich.program = t.command_line.program; subcommand; args }
         | Error _ ->
           failwith
             "reentrant query was invoked with an invalid command, which the completion \
              script should never do")
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
    let t = preprocess_help t in
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
    let* traverse_ = traverse t raw_command_line in
    let command_line = traverse_to_rich_command_line traverse_ in
    let arg_parser =
      match traverse_.operation with
      | `Arg_parser arg_parser ->
        (* This is the common case. Run the selected argument parser
           which will usually have the side effect of running the user's
           program logic. *)
        arg_parser
      | `Print_completion_script_bash ->
        (* Print the completion script. Note that this can't be combined
           into the regular parser logic because it needs to know the
           completion spec, which isn't available to regular argument
           parsers. *)
        Arg_parser.map'
          finalized_print_completion_script_bash_arg_parser
          ~f:
            (fun
              { Completion_config.program_name
              ; program_exe_for_reentrant_query
              ; global_symbol_prefix
              ; command_hash_in_function_names
              ; options
              }
            ->
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
                 }))
    in
    let traverse_help =
      Option.bind traverse_.traverse_help ~f:(fun { subcommand_up_to_help; _ } ->
        if List.equal ~eq:String.equal subcommand_up_to_help traverse_.subcommand
        then None
        else traverse_.traverse_help)
    in
    Arg_parser.Private.eval
      arg_parser
      ~command_line
      ~ignore_errors:false
      ~alt_subcommand_for_usage:
        (Option.map traverse_help ~f:(fun { subcommand_excluding_help; _ } ->
           subcommand_excluding_help))
      ~alt_subcommand_for_errors:
        (Option.map traverse_help ~f:(fun { subcommand_up_to_help; _ } ->
           subcommand_up_to_help))
  ;;

  (* All the side effects that can be requested by a parser. Does not return.
     [test_friendly] prevents this function from exiting the program and prints
     all of its output to stdout. *)
  let handle_non_ret non_ret ~(help_style : Help_style.t) ~version ~test_friendly =
    let () =
      match (non_ret : Non_ret.t) with
      | Help { command_doc_spec; error; message } ->
        (match message with
         | None -> ()
         | Some message ->
           if error
           then (
             let ppf = Format.err_formatter in
             Ansi_style.pp_with_style help_style.error ppf ~f:(fun ppf ->
               Format.pp_print_string ppf "Error: ");
             Format.pp_print_string ppf message;
             Format.pp_print_newline ppf ())
           else (
             Format.pp_print_string Format.std_formatter message;
             Format.pp_print_newline Format.std_formatter ());
           Format.pp_print_newline Format.std_formatter ());
        Help.pp help_style Format.std_formatter command_doc_spec;
        if error then exit 1
      | Manpage { prose; command_doc_spec } ->
        let manpage = { Manpage.spec = command_doc_spec; prose; version } in
        print_endline (Manpage.to_troff_string manpage)
      | Reentrant_query { suggestions } -> List.iter suggestions ~f:print_endline
      | Parse_error { error; command_doc_spec } ->
        if test_friendly
        then print_endline (Parse_error.to_string error)
        else (
          let ppf = Format.err_formatter in
          Ansi_style.pp_with_style help_style.error ppf ~f:(fun ppf ->
            Format.pp_print_string ppf "Error: ");
          Format.pp_print_string ppf (Parse_error.to_string error);
          Format.pp_force_newline ppf ();
          Format.pp_force_newline ppf ();
          Help.pp_usage help_style ppf command_doc_spec;
          Format.pp_force_newline ppf ();
          Format.pp_print_string
            ppf
            (sprintf
               "For more info, try running `%s`."
               (String.concat
                  ((command_doc_spec.program_name :: command_doc_spec.subcommand)
                   @ [ Name.to_string_with_dashes Built_in.help_long ])
                  ~sep:" "));
          Format.pp_force_newline ppf ();
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
  let print_help_spec_with_style style spec = Help.pp style Format.std_formatter spec

  let print_manpage spec prose =
    let manpage = { Manpage.spec; prose; version = None } in
    print_endline (Manpage.to_troff_string manpage)
  ;;
end
