open! Import
open Shell_dsl

module Options = struct
  type t =
    { no_comments : bool
    ; minify_global_names : bool
    ; no_whitespace : bool
    }

  let default =
    { no_comments = false; minify_global_names = false; no_whitespace = false }
  ;;
end

module Status = struct
  open Global_named_value

  let done_value = "100"
  let error_word_out_of_bounds_value = "101"
  let error_word_index_past_cursor_value = "102"
  let done_ = global_variable ~name:"STATUS_DONE" ~initial_value:done_value

  let error_word_out_of_bounds =
    global_variable
      ~name:"STATUS_ERROR_WORD_OUT_OF_BOUNDS"
      ~initial_value:error_word_out_of_bounds_value
  ;;

  let error_word_index_past_cursor =
    global_variable
      ~name:"WORD_INDEX_PAST_CURSOR"
      ~initial_value:error_word_index_past_cursor_value
  ;;

  let is_error =
    let open Stmt in
    function_
      "status_is_error"
      [ test_raw_cond_of_string_with_global_name
          ~f:(sprintf "\"$1\" -gt \"$%s\"")
          (name done_)
      ]
  ;;

  let all_global_values =
    [ done_; error_word_out_of_bounds; error_word_index_past_cursor; is_error ]
  ;;
end

module Error = struct
  open Global_named_value

  let print =
    let open Stmt in
    function_ "error_print" [ raw "echo \"$1\" > /dev/stderr" ]
  ;;

  let all_global_values = [ print ]
end

module Comp_words = struct
  open Global_named_value

  let count =
    let open Stmt in
    function_ "comp_words_count" [ raw "echo \"${#COMP_WORDS[@]}\"" ]
  ;;

  let get_nth =
    let open Stmt in
    function_
      "comp_words_get_nth"
      [ raw "local i=$1"
      ; if_
          (Cond.test_raw_of_string_with_global_name
             ~f:(sprintf "\"$i\" -ge \"$(%s)\"")
             (name count))
          [ return (Value.global Status.error_word_out_of_bounds) ]
      ; raw "echo \"${COMP_WORDS[$i]}\""
      ]
  ;;

  module Traverse = struct
    let current_index =
      global_variable ~name:"COMP_WORDS_CURRENT_INDEX" ~initial_value:"0"
    ;;

    let init =
      let open Stmt in
      function_
        "comp_words_traverse_init"
        [ raw_with_global_name ~f:(sprintf "%s=0") (name current_index) ]
    ;;

    let get_current =
      let open Stmt in
      function_
        "comp_words_traverse_get_current"
        [ call get_nth [ Value.global current_index ] ]
    ;;

    let advance =
      let open Stmt in
      function_
        "comp_words_traverse_advance"
        [ raw_with_global_name
            ~f:(fun v -> sprintf "%s=$((%s + 1))" v v)
            (name current_index)
        ]
    ;;

    let is_at_cursor =
      let open Stmt in
      function_
        "comp_words_traverse_is_at_cursor"
        [ raw_with_global_name
            ~f:(sprintf "test \"$%s\" -eq \"$COMP_CWORD\"")
            (name current_index)
        ]
    ;;

    let is_past_cursor =
      let open Stmt in
      function_
        "comp_words_traverse_is_past_cursor"
        [ raw_with_global_name
            ~f:(sprintf "test \"$%s\" -gt \"$COMP_CWORD\"")
            (name current_index)
        ]
    ;;

    let all_global_values =
      [ current_index; init; get_current; advance; is_at_cursor; is_past_cursor ]
    ;;
  end

  let all_global_values = count :: get_nth :: Traverse.all_global_values
end

module Add_reply = struct
  open Global_named_value

  let files =
    let open Stmt in
    function_
      "add_reply_files"
      [ comment
          "Takes the word under the cursor (just the portion up to the cursor) and \
           completes with files in the current directory."
      ; raw "COMPREPLY+=($(compgen -A file -- \"$1\"))"
      ]
  ;;

  let fixed =
    let open Stmt in
    function_
      "add_reply_fixed"
      [ comment
          "Takes the word under the cursor (just the portion up to the cursor) and a \
           space separated list of completion strings."
      ; raw "COMPREPLY+=($(compgen -W \"$2\" -- \"$1\"))"
      ]
  ;;

  let all_global_values = [ files; fixed ]
end

module Reentrant_query = struct
  open Global_named_value

  let run ~program_exe ~print_reentrant_completions_name =
    let open Stmt in
    function_
      "reentrant_query_run"
      [ comment
          "Takes a reentrant query index and the current word under the cursor (up to \
           the cursor). It invokes the program with the given subcommand path and some \
           special arguments that cause it to emit the result of the requested query. \
           The result is then added to COMPREPLY."
      ; raw "local query_index=$1"
      ; raw "local current_word=$2"
      ; raw "local command suggestions"
      ; raw
          (sprintf
             "command=\"%s %s=$query_index -- $COMP_LINE\""
             program_exe
             (Name.to_string_with_dashes print_reentrant_completions_name))
      ; raw "suggestions=$(eval \"$command\")"
      ; raw "COMPREPLY+=($(compgen -W \"$suggestions\" -- \"$current_word\"))"
      ]
  ;;
end

let hint_add_reply (hint : int Completion_spec.Hint.t) ~reentrant_query_run ~current_word =
  let open Stmt in
  match hint with
  | File -> call Add_reply.files [ current_word ]
  | Values values ->
    call Add_reply.fixed [ current_word; Value.literal (String.concat ~sep:" " values) ]
  | Reentrant query_index ->
    call reentrant_query_run [ Value.literal (string_of_int query_index); current_word ]
;;

module Subcommand_and_positional_arg_completion = struct
  open Global_named_value

  let function_name ~subcommand_path ~command_hash_in_function_names =
    let prefix =
      if command_hash_in_function_names
      then (
        (* Add the hash of the name to the function name to avoid
           collisions between function names *)
        let hash = Hashtbl.hash subcommand_path in
        sprintf "hash_%d__" hash)
      else ""
    in
    sprintf "%s%s" prefix (String.concat ~sep:"__" (List.rev subcommand_path))
  ;;

  let functions
    ~(spec : _ Completion_spec.t)
    ~subcommand_path
    ~reentrant_query_run
    ~command_hash_in_function_names
    =
    let open Stmt in
    let base_function_name =
      function_name ~subcommand_path ~command_hash_in_function_names
    in
    let complete_positional_args_function =
      let stmt_of_hint =
        hint_add_reply ~reentrant_query_run ~current_word:(Value.literal "$1")
      in
      let cases =
        List.mapi spec.parser_spec.positional_args_hints.finite_args ~f:(fun i hint ->
          let stmt =
            match hint with
            | Some hint -> stmt_of_hint hint
            | None -> noop
          in
          Case_pattern.singleton @@ string_of_int i, [ stmt ])
        @
        match spec.parser_spec.positional_args_hints.repeated_arg with
        | None -> []
        | Some `No_hint -> [ Case_pattern.singleton "*", [ noop ] ]
        | Some (`Hint hint) -> [ Case_pattern.singleton "*", [ stmt_of_hint hint ] ]
      in
      if List.is_empty cases
      then None
      else
        Some
          (function_
             (sprintf "%s__complete_positional_args" base_function_name)
             [ comment
                 "Takes the portion of the word under the cursor before the cursor and \
                  the index of the current positional argument on the command line and \
                  adds comp replies for that positional argument begining with that \
                  prefix."
             ; case (Value.literal "$2") cases
             ])
    in
    List.filter_opt
      [ complete_positional_args_function
      ; Some
          (let named_arg_cases =
             let named_arguments_with_hints =
               List.filter_map
                 spec.parser_spec.named_args
                 ~f:(fun (named_arg : _ Completion_spec.Named_arg.t) ->
                   if named_arg.has_param
                   then
                     Option.map named_arg.hint ~f:(fun hint ->
                       let stmts =
                         [ comment
                             (sprintf
                                "completions for: %s %s"
                                (String.concat ~sep:" " subcommand_path)
                                (Name.to_string_with_dashes
                                   (Completion_spec.Named_arg.first_name named_arg)))
                         ; hint_add_reply
                             hint
                             ~reentrant_query_run
                             ~current_word:(Value.literal "$2")
                         ; return (Value.global Status.done_)
                         ]
                       in
                       Completion_spec.Named_arg.to_patterns_with_dashes named_arg, stmts)
                   else None)
             in
             (* Arguments with values but no hints. The completion script will
                default to file completions in this case. *)
             let named_arguments_without_hints =
               let patterns =
                 List.filter_map
                   spec.parser_spec.named_args
                   ~f:(fun (named_arg : _ Completion_spec.Named_arg.t) ->
                     if named_arg.has_param && Option.is_none named_arg.hint
                     then
                       Some (Completion_spec.Named_arg.to_patterns_with_dashes named_arg)
                     else None)
               in
               Nonempty_list.of_list patterns
               |> Option.map ~f:(fun patterns ->
                 ( Case_pattern.union patterns
                 , [ comment "case for named arguments without hints"
                   ; call Add_reply.files [ Value.literal "$2" ]
                   ; return (Value.global Status.done_)
                   ] ))
               |> Option.to_list
             in
             named_arguments_without_hints @ named_arguments_with_hints
           in
           let subcommand_cases =
             let named_args =
               List.map
                 spec.parser_spec.named_args
                 ~f:Completion_spec.Named_arg.to_patterns_with_dashes
               |> Nonempty_list.of_list
               |> Option.map ~f:(fun patterns ->
                 ( Case_pattern.union patterns
                 , [ raw "prev_word_was_named_argument_with_value=1" ] ))
               |> Option.to_list
             in
             let subcommands =
               List.map
                 spec.subcommands
                 ~f:(fun (subcommand : _ Completion_spec.subcommand) ->
                   let subcommand_path = subcommand.name :: subcommand_path in
                   let completion_function_name =
                     Global_name.make
                       (function_name ~subcommand_path ~command_hash_in_function_names)
                   in
                   let stmts =
                     [ raw_with_global_name
                         ~f:(sprintf "%s \"$1\" \"$2\" \"$3\"")
                         completion_function_name
                     ; return (Value.literal "$?")
                     ]
                   in
                   Case_pattern.singleton subcommand.name, stmts)
             in
             named_args
             @ subcommands
             @ [ ( Case_pattern.singleton "-*"
                 , [ comment "Ignore other words that look like arguments"
                   ; raw "prev_word_was_named_argument_with_value=0"
                   ] )
               ; ( Case_pattern.singleton "*"
                 , [ if_
                       (Cond.test_raw
                          "\"$prev_word_was_named_argument_with_value\" -eq 0")
                       [ raw "positional_argument_index=$((positional_argument_index+1))"
                       ]
                   ; raw "prev_word_was_named_argument_with_value=0"
                   ] )
               ]
           in
           function_
             base_function_name
             [ raw "local prev_word_was_named_argument_with_value=0"
             ; raw "local positional_argument_index=0"
             ; while_
                 Cond.true_
                 [ if_
                     (Cond.call Comp_words.Traverse.is_past_cursor [])
                     [ return (Value.global Status.error_word_index_past_cursor) ]
                 ; if_
                     (Cond.call Comp_words.Traverse.is_at_cursor [])
                     [ comment
                         "Try to complete subcommands and positional arguments first."
                     ; (match spec.subcommands with
                        | [] ->
                          comment
                            "This is where we would add completions for subcommands \
                             however this command has no subcommands."
                        | subcommands ->
                          let space_separated_subcommands =
                            List.map
                              subcommands
                              ~f:(fun (subcommand : _ Completion_spec.subcommand) ->
                                subcommand.name)
                            |> String.concat ~sep:" "
                          in
                          call
                            Add_reply.fixed
                            [ Value.literal "$2"
                            ; Value.literal space_separated_subcommands
                            ])
                     ; (match complete_positional_args_function with
                        | Some complete_positional_args_function ->
                          call
                            complete_positional_args_function
                            [ Value.literal "$2"
                            ; Value.literal "$positional_argument_index"
                            ]
                        | None ->
                          comment
                            "This is where we would add completions for positional \
                             arguments, however this command has no positional arguments")
                     ; if_
                         (Cond.test_raw "\"${#COMPREPLY[@]}\" == \"0\"")
                         [ comment
                             "If there were no suggestions for subcommands or positional \
                              arguments, try completing named arguments instead."
                         ; (let space_separated_names =
                              List.append
                                (Completion_spec.Parser_spec
                                 .all_long_names_with_dashes_sorted
                                   spec.parser_spec)
                                (Completion_spec.Parser_spec
                                 .all_short_names_with_dashes_sorted
                                   spec.parser_spec)
                              |> String.concat ~sep:" "
                            in
                            call
                              Add_reply.fixed
                              [ Value.literal "$2"; Value.literal space_separated_names ])
                         ]
                     ; return (Value.global Status.done_)
                     ]
                     ~else_:
                       [ comment
                           "Avoid the variable name \"status\" as it's reserved by some \
                            shells."
                       ; raw "local current_word status_"
                       ; raw_with_global_name
                           ~f:(sprintf "current_word=$(%s)")
                           (name Comp_words.Traverse.get_current)
                       ; raw "status_=$?"
                       ; if_
                           (Cond.test_raw "\"$status_\" -ne 0")
                           [ return (Value.literal "$status_") ]
                       ; call Comp_words.Traverse.advance []
                       ; if_
                           (Cond.call Comp_words.Traverse.is_past_cursor [])
                           [ comment
                               "Bounds check to catch errors in the implementation of \
                                the completion script"
                           ; return (Value.global Status.error_word_index_past_cursor)
                           ]
                       ; if_
                           (Cond.call Comp_words.Traverse.is_at_cursor [])
                           [ comment
                               "The parser has reached the word under the cursor. \
                                Attempt to complete it and then exit."
                           ; case (Value.literal "$current_word") named_arg_cases
                           ]
                       ; case (Value.literal "$current_word") subcommand_cases
                       ]
                 ]
             ])
      ]
  ;;
end

module Completion_entry_point = struct
  open Global_named_value

  let function_ ~command_hash_in_function_names =
    let open Stmt in
    let completion_root_name =
      Global_name.make
        (Subcommand_and_positional_arg_completion.function_name
           ~subcommand_path:[]
           ~command_hash_in_function_names)
    in
    function_
      "complete"
      [ call Comp_words.Traverse.init []
      ; if_
          (Cond.test_raw "\"$COMP_CWORD\" == \"0\"")
          [ call
              Error.print
              [ Value.literal
                  "Unexpected \\$COMP_CWORD value of 0. $COMP_CWORD should be at least 1 \
                   as the shell uses the first word of the command line to determine \
                   which completion script to run."
              ]
          ]
          ~elifs:
            [ ( Cond.test_raw_of_string_with_global_name
                  ~f:(sprintf "\"$(%s)\" -lt 2")
                  (name Comp_words.count)
              , [ call
                    Error.print
                    [ Value.literal
                        "Unexpected length of \\$COMP_WORDS array: ${#COMP_WORDS[@]}. \
                         Its length should be at least 2 since the first element should \
                         always be the program name, and the second element will be the \
                         first word after the program name, which is expected to be the \
                         empty string if no additional words have been entered after the \
                         program name."
                    ]
                ] )
            ]
          ~else_:
            [ call Comp_words.Traverse.advance []
            ; raw_with_global_name
                ~f:(sprintf "%s \"$1\" \"$2\" \"$3\"")
                completion_root_name
            ; case
                (Value.literal "$?")
                [ Case_pattern.singleton Status.done_value, [ noop ]
                ; ( Case_pattern.singleton Status.error_word_index_past_cursor_value
                  , [ call
                        Error.print
                        [ Value.literal
                            "Unexpected error in completion script: Traversed command \
                             line beyond the current cursor position"
                        ]
                    ] )
                ; ( Case_pattern.singleton Status.error_word_out_of_bounds_value
                  , [ call
                        Error.print
                        [ Value.literal
                            "Unexpected error in completion script: Traversed beyond the \
                             end of the command line"
                        ]
                    ] )
                ; ( Case_pattern.singleton "*"
                  , [ call
                        Error.print
                        [ Value.literal "Unknown error in completion script" ]
                    ] )
                ]
            ]
      ]
  ;;
end

let rec functions_of_spec
  (spec : _ Completion_spec.t)
  ~subcommand_path
  ~reentrant_query_run
  ~command_hash_in_function_names
  =
  let subcommand_and_positional_arg_completion =
    Subcommand_and_positional_arg_completion.functions
      ~spec
      ~subcommand_path
      ~reentrant_query_run
      ~command_hash_in_function_names
  in
  let subcommand_completions =
    List.concat_map
      spec.subcommands
      ~f:(fun (subcommand : _ Completion_spec.subcommand) ->
        let subcommand_path = subcommand.name :: subcommand_path in
        functions_of_spec
          subcommand.spec
          ~subcommand_path
          ~reentrant_query_run
          ~command_hash_in_function_names)
  in
  subcommand_completions @ subcommand_and_positional_arg_completion
;;

let bash_header ~program_name ~global_symbol_prefix =
  let open Stmt in
  [ raw "#!/usr/bin/env bash"
  ; comment (sprintf "Completion script for %s. Generated by climate." program_name)
  ]
  |> List.map ~f:(Bash.stmt_to_string ~global_symbol_prefix)
  |> String.concat ~sep:"\n"
;;

let make_random_prefix () =
  Random.self_init ();
  sprintf "__climate_complete_%d__" (Random.int32 Int32.max_int |> Int32.to_int)
;;

module Short_symbol = struct
  type t = int list

  let initial : t = [ 0 ]

  let allowed_chars =
    ('_' :: List.init ~len:26 ~f:(fun i -> Char.chr (Char.code 'a' + i)))
    @ List.init ~len:26 ~f:(fun i -> Char.chr (Char.code 'A' + i))
    |> Array.of_list
  ;;

  let num_allowed_chars = Array.length allowed_chars

  let rec next = function
    | [] -> [ 1 ]
    | x :: xs ->
      let x = x + 1 in
      if Int.equal x num_allowed_chars then 0 :: next xs else x :: xs
  ;;

  let to_string t =
    String.init (List.length t) ~f:(fun i -> Array.get allowed_chars (List.nth t i))
  ;;
end

let post_process_globals { Options.no_comments; minify_global_names; _ } globals =
  let globals =
    if no_comments
    then
      List.map
        globals
        ~f:
          (Global_named_value.with_function_stmts
             ~f:
               (Stmt.transform_blocks_top_down
                  ~f:(List.filter ~f:(Fun.negate Stmt.is_comment))))
    else globals
  in
  let globals =
    if minify_global_names
    then (
      let short_symbol_table =
        let all_global_name_suffixes =
          List.map globals ~f:(fun global_named_value ->
            Global_named_value.name global_named_value |> Global_name.suffix)
        in
        List.fold_left
          all_global_name_suffixes
          ~init:(Short_symbol.initial, [])
          ~f:(fun (current, acc) suffix ->
            let next = Short_symbol.next current in
            next, (suffix, Short_symbol.to_string current) :: acc)
        |> snd
        |> List.rev
        |> List.tl
        |> String.Map.of_list
        |> Result.get_ok
      in
      let transform_suffix suffix =
        match String.Map.find short_symbol_table suffix with
        | None -> suffix
        | Some suffix -> suffix
      in
      List.map globals ~f:(fun global ->
        let global = Global_named_value.with_suffix global ~f:transform_suffix in
        let global =
          Global_named_value.with_function_stmts
            global
            ~f:(Stmt.map_global_name_suffix ~f:transform_suffix)
        in
        global))
    else globals
  in
  globals
;;

(* Generate a bash completion script, expected to be sourced in a
   shell to enable tab completion according to a provided spec. The
   script defines a completion function which is registered with the
   shell. The contract between this completion function and the shell
   is as follows:

   The function takes three arguments:
   $1 is the name of the command whose arguments are being completed
   $2 is the current word being completed up to the cursor
   $3 is the word preceding the current word being completed

   Additionally the function expects some global variables to be
   defined:
   $COMP_WORDS is an array of all the words in the current command line
   $COMP_LINE is a string containing the current command line
   $COMP_CWORD is an index into $COMP_WORDS of the word under the cursor

   The function also expects a global variable $COMPREPLY to be
   defined, and it will populate this array with completion
   suggestions.
*)
let generate_bash
  spec
  ~program_name
  ~program_exe_for_reentrant_query
  ~print_reentrant_completions_name
  ~global_symbol_prefix
  ~command_hash_in_function_names
  ~(options : Options.t)
  =
  let spec = Completion_spec.replace_reentrants_with_indices spec in
  let all_functions =
    let entry_point = Completion_entry_point.function_ ~command_hash_in_function_names in
    let reentrant_query_run =
      let program_exe =
        match program_exe_for_reentrant_query with
        | `Program_name -> program_name
        | `Other program_exe -> program_exe
      in
      Reentrant_query.run ~program_exe ~print_reentrant_completions_name
    in
    let static_global_values =
      Status.all_global_values
      @ Error.all_global_values
      @ Comp_words.all_global_values
      @ Add_reply.all_global_values
      @ [ reentrant_query_run ]
    in
    let completion_functions =
      functions_of_spec
        spec
        ~subcommand_path:[]
        ~reentrant_query_run
        ~command_hash_in_function_names
    in
    post_process_globals
      options
      (entry_point :: (static_global_values @ completion_functions))
  in
  let global_symbol_prefix =
    match global_symbol_prefix with
    | `Random -> make_random_prefix ()
    | `Custom s -> s
  in
  String.concat
    ~sep:(if options.no_whitespace then "\n" else "\n\n")
    ([ bash_header ~program_name ~global_symbol_prefix ]
     @ List.map all_functions ~f:(Bash.global_named_value_to_string ~global_symbol_prefix)
     @ [ Stmt.raw_with_global_name
           (Global_named_value.name (List.hd all_functions))
           ~f:(fun complete_entry ->
             sprintf "complete -F %s %s" complete_entry program_name)
         |> Bash.stmt_to_string ~global_symbol_prefix
       ])
;;
