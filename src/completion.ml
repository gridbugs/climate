open! Import
open Shell_dsl

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

module Named_arg_value_completion = struct
  open Global_named_value

  let function_name
    ~(named_arg : _ Completion_spec.Named_arg.t)
    ~subcommand_path
    ~command_hash_in_function_names
    =
    let prefix =
      if command_hash_in_function_names
      then (
        (* Add the hash of the name to the function name to avoid
           collisions between function names *)
        let hash = Hashtbl.hash (named_arg, subcommand_path) in
        sprintf "hash_%d__" hash)
      else ""
    in
    sprintf
      "%s%s_%s"
      prefix
      (String.concat ~sep:"__" (List.rev subcommand_path))
      (Name.to_string_with_dashes named_arg.name)
  ;;

  (* Generates function for completing the argument to a particular
     named argument to a particular subcommand *)
  let function_
    ~(named_arg : _ Completion_spec.Named_arg.t)
    ~subcommand_path
    ~reentrant_query_run
    ~command_hash_in_function_names
    =
    let open Stmt in
    let hints =
      Option.map
        named_arg.hint
        ~f:
          (hint_add_reply
             ~reentrant_query_run
             ~current_word:(Value.literal "$current_word_up_to_cursor"))
      |> Option.to_list
    in
    function_
      (function_name ~named_arg ~subcommand_path ~command_hash_in_function_names)
      [ raw "local current_word_up_to_cursor=$2"
      ; if_
          (Cond.call Comp_words.Traverse.is_past_cursor [])
          [ return (Value.global Status.error_word_index_past_cursor) ]
      ; if_
          (Cond.call Comp_words.Traverse.is_at_cursor [])
          ((comment "The cursor is on the parameter of the named argument." :: hints)
           @ [ return (Value.global Status.done_) ])
      ]
  ;;
end

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
    let complete_named_args_function =
      function_
        (sprintf "%s:complete_named_args" base_function_name)
        [ comment
            "Takes the portion of the word under the cursor before the cursor and adds \
             comp replies for all named arguments begining with that prefix."
        ; (let space_separated_names =
             Completion_spec.named_args_sorted spec
             |> List.map ~f:(fun (named_arg : _ Completion_spec.Named_arg.t) ->
               Name.to_string_with_dashes named_arg.name)
             |> String.concat ~sep:" "
           in
           call
             Add_reply.fixed
             [ Value.literal "$1"; Value.literal space_separated_names ])
        ]
    in
    let complete_subcommands_function =
      function_
        (sprintf "%s:complete_subcommands" base_function_name)
        [ comment
            "Takes the portion of the word under the cursor before the cursor and adds \
             comp replies for all subcommands begining with that prefix."
        ; (let space_separated_subcommands =
             List.map
               spec.subcommands
               ~f:(fun (subcommand : _ Completion_spec.subcommand) -> subcommand.name)
             |> String.concat ~sep:" "
           in
           call
             Add_reply.fixed
             [ Value.literal "$1"; Value.literal space_separated_subcommands ])
        ]
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
          string_of_int i, [ stmt ])
        @
        match spec.parser_spec.positional_args_hints.repeated_arg with
        | None -> []
        | Some `No_hint -> [ "*", [ noop ] ]
        | Some (`Hint hint) -> [ "*", [ stmt_of_hint hint ] ]
      in
      function_
        (sprintf "%s:complete_positional_args" base_function_name)
        [ comment
            "Takes the portion of the word under the cursor before the cursor and the \
             index of the current positional argument on the command line and adds comp \
             replies for that positional argument begining with that prefix."
        ; case (Value.literal "$2") cases
        ]
    in
    [ complete_named_args_function
    ; complete_subcommands_function
    ; complete_positional_args_function
    ; (let cases =
         List.map spec.subcommands ~f:(fun (subcommand : _ Completion_spec.subcommand) ->
           let subcommand_path = subcommand.name :: subcommand_path in
           let completion_function_name =
             Global_name.with_prefix
               (function_name ~subcommand_path ~command_hash_in_function_names)
           in
           let stmts =
             [ raw_with_global_name
                 ~f:(sprintf "%s \"$1\" \"$2\" \"$3\"")
                 completion_function_name
             ; return (Value.literal "$?")
             ]
           in
           subcommand.name, stmts)
         @ (Completion_spec.named_args_sorted spec
            |> List.filter_map ~f:(fun (named_arg : _ Completion_spec.Named_arg.t) ->
              if named_arg.has_param
              then (
                let completion_function_name =
                  Global_name.with_prefix
                    (Named_arg_value_completion.function_name
                       ~named_arg
                       ~subcommand_path
                       ~command_hash_in_function_names)
                in
                let stmts =
                  [ raw_with_global_name
                      ~f:(sprintf "%s \"$1\" \"$2\" \"$3\"")
                      completion_function_name
                  ; raw "status=$?"
                  ; if_
                      (Cond.test_raw "\"$status\" -ne 0")
                      [ return (Value.literal "$status") ]
                  ; raw "prev_word_was_named_argument_with_value=1"
                  ]
                in
                Some (Name.to_string_with_dashes named_arg.name, stmts))
              else None))
         @ [ ( "-*"
             , [ comment "Ignore other words that look like arguments"
               ; raw "prev_word_was_named_argument_with_value=0"
               ] )
           ; ( "*"
             , [ if_
                   (Cond.test_raw "\"$prev_word_was_named_argument_with_value\" -eq 0")
                   [ raw "positional_argument_index=$((positional_argument_index+1))" ]
               ; raw "prev_word_was_named_argument_with_value=0"
               ] )
           ]
       in
       function_
         base_function_name
         [ raw "local current_word_up_to_cursor=$2"
         ; raw "local prev_word_was_named_argument_with_value=0"
         ; raw "local positional_argument_index=0"
         ; while_
             Cond.true_
             [ if_
                 (Cond.call Comp_words.Traverse.is_past_cursor [])
                 [ return (Value.global Status.error_word_index_past_cursor) ]
             ; if_
                 (Cond.call Comp_words.Traverse.is_at_cursor [])
                 [ comment "Try to complete subcommands and positional arguments first."
                 ; call
                     complete_subcommands_function
                     [ Value.literal "$current_word_up_to_cursor" ]
                 ; call
                     complete_positional_args_function
                     [ Value.literal "$current_word_up_to_cursor"
                     ; Value.literal "$positional_argument_index"
                     ]
                 ; if_
                     (Cond.test_raw "\"${#COMPREPLY[@]}\" == \"0\"")
                     [ comment
                         "If there were no suggestions for subcommands or positional \
                          arguments, try completing named arguments instead."
                     ; call
                         complete_named_args_function
                         [ Value.literal "$current_word_up_to_cursor" ]
                     ]
                 ; return (Value.global Status.done_)
                 ]
                 ~else_:
                   [ raw "local current_word status"
                   ; raw_with_global_name
                       ~f:(sprintf "current_word=$(%s)")
                       (name Comp_words.Traverse.get_current)
                   ; raw "status=$?"
                   ; if_
                       (Cond.test_raw "\"$status\" -ne 0")
                       [ return (Value.literal "$status") ]
                   ; call Comp_words.Traverse.advance []
                   ; case (Value.literal "$current_word") cases
                   ]
             ]
         ])
    ]
  ;;
end

module Completion_entry_point = struct
  open Global_named_value

  let function_ ~program_name ~command_hash_in_function_names =
    let open Stmt in
    let completion_root_name =
      Global_name.with_prefix
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
                        "Unexpected length of \\$COMP_WORDS array: $(${#COMP_WORDS[@]}). \
                         Its length should be at least 2 since the first element should \
                         always be the program name, and the second element will be the \
                         first word after the program name, which is expected to be the \
                         empty string if no additional words have been entered after the \
                         program name."
                    ]
                ] )
            ; ( Cond.test_raw_of_string_with_global_name
                  ~f:(fun comp_words_traverse_get_current ->
                    sprintf
                      "\"$(%s)\" != \"%s\""
                      comp_words_traverse_get_current
                      program_name)
                  (name Comp_words.Traverse.get_current)
              , [ call
                    Error.print
                    [ Value.literal_with_global_name
                        ~f:(fun comp_words_traverse_get_current ->
                          sprintf
                            "Completion script found unexpected first word of command \
                             line: '$(%s)'. This is the completion script for the \
                             program '%s' and so it's expected that the first word of \
                             the command line will be '%s'."
                            comp_words_traverse_get_current
                            program_name
                            program_name)
                        (name Comp_words.Traverse.get_current)
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
                [ Status.done_value, [ noop ]
                ; ( Status.error_word_index_past_cursor_value
                  , [ call
                        Error.print
                        [ Value.literal
                            "Unexpected error in completion script: Traversed command \
                             line beyond the current cursor position"
                        ]
                    ] )
                ; ( Status.error_word_out_of_bounds_value
                  , [ call
                        Error.print
                        [ Value.literal
                            "Unexpected error in completion script: Traversed beyond the \
                             end of the command line"
                        ]
                    ] )
                ; ( "*"
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
  let named_arg_completion_functions =
    List.filter_map
      spec.parser_spec.named_args
      ~f:(fun (named_arg : _ Completion_spec.Named_arg.t) ->
        if named_arg.has_param
        then
          Some
            (Named_arg_value_completion.function_
               ~named_arg
               ~subcommand_path
               ~reentrant_query_run
               ~command_hash_in_function_names)
        else None)
  in
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
  subcommand_completions
  @ named_arg_completion_functions
  @ subcommand_and_positional_arg_completion
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

let generate_bash
  spec
  ~program_name
  ~program_exe_for_reentrant_query
  ~print_reentrant_completions_name
  ~global_symbol_prefix
  ~command_hash_in_function_names
  =
  let spec = Completion_spec.replace_reentrants_with_indices spec in
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
  let all_functions = static_global_values @ completion_functions in
  let global_symbol_prefix =
    match global_symbol_prefix with
    | `Random -> make_random_prefix ()
    | `Custom s -> s
  in
  let header = bash_header ~program_name ~global_symbol_prefix in
  let globals =
    List.map all_functions ~f:(Bash.global_named_value_to_string ~global_symbol_prefix)
  in
  let entry_point =
    Completion_entry_point.function_ ~program_name ~command_hash_in_function_names
    |> Bash.global_named_value_to_string ~global_symbol_prefix
  in
  let last_line =
    Stmt.raw_with_global_name
      (Global_named_value.name
         (Completion_entry_point.function_ ~program_name ~command_hash_in_function_names))
      ~f:(fun complete_entry -> sprintf "complete -F %s %s" complete_entry program_name)
    |> Bash.stmt_to_string ~global_symbol_prefix
  in
  (header :: globals) @ [ entry_point; last_line ] |> String.concat ~sep:"\n\n"
;;
