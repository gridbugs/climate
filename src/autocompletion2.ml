open! Import

let sprintf = Printf.sprintf

type global_name = unique_prefix:string -> string

let name_with_prefix ~unique_prefix name = String.cat unique_prefix name

type global_named_value =
  | Global_variable of
      { name : global_name
      ; initial_value : string
      }
  | Function of
      { name : global_name
      ; body : stmt list
      }

and function_call = global_named_value * value list

and value =
  | Literal of string
  | Global_ of global_named_value
  | Call_output of function_call

and condition =
  | True
  | False
  | Call_status of function_call

and expr =
  | Raw of string
  | Raw_with_global_named_value of ((string -> string) * global_named_value)
    (* An expr which includes a reference to a given named value. The
       first parameter is a passed the final name of the given
       function and returns the raw code to be emitted. *)
  | Call of (global_named_value * expr list)
  | Global of global_named_value

and stmt =
  | Expr of expr
  | Raw_stmt of string
  | Raw_stmt_with_global_named_value of ((string -> string) * global_named_value)
    (* A stmt which includes a reference to a given named value. The
       first parameter is a passed the final name of the given
       function and returns the raw code to be emitted. *)
  | If of (expr * stmt list)
  | If_else of ((expr * stmt list) Nonempty_list.t * stmt list)
  | Case of (expr * (string * stmt list) list)
  | While of (expr * stmt list)
  | For of ((string * expr) * stmt list)
  | Return of expr
  | Comment of string

let stmt_of_expr stmt = Expr stmt

let global_named_value_global_name = function
  | Global_variable { name; _ } | Function { name; _ } -> name
;;

let global_variable name initial_value =
  Global_variable { name = name_with_prefix name; initial_value }
;;

let function_ name body = Function { name = name_with_prefix name; body }

module Status = struct
  let done_value = "100"
  let error_word_out_of_bounds_value = "101"
  let error_word_index_past_cursor_value = "102"
  let done_ = global_variable "STATUS_DONE" done_value

  let error_word_out_of_bounds =
    global_variable "STATUS_ERROR_WORD_OUT_OF_BOUNDS" error_word_out_of_bounds_value
  ;;

  let error_word_index_past_cursor =
    global_variable "WORD_INDEX_PAST_CURSOR" error_word_index_past_cursor_value
  ;;

  let is_error =
    function_
      "status_is_error"
      [ Raw_with_global_named_value (sprintf "test \"$1\" -gt \"$%s\"", done_)
        |> stmt_of_expr
      ]
  ;;

  let all_global_values =
    [ done_; error_word_out_of_bounds; error_word_index_past_cursor; is_error ]
  ;;
end

module Error = struct
  let print = function_ "error_print" [ Raw_stmt "echo $1 > /dev/stderr" ]
  let all_global_values = [ print ]
end

module Comp_words = struct
  let count = function_ "comp_words_count" [ Raw_stmt "echo \"${#COMP_WORDS[@]}\"" ]

  let get_nth =
    function_
      "comp_words_get_nth"
      [ Raw_stmt "local i=$1"
      ; If
          ( Raw_with_global_named_value (sprintf "[ \"$i\" -ge \"$(%s)\"", count)
          , [ Return (Global Status.error_word_out_of_bounds) ] )
      ; Raw_stmt "echo \"${COMP_WORDS[$i]}\""
      ]
  ;;

  module Traverse = struct
    let current_index = global_variable "COMP_WORDS_CURRENT_INDEX" "0"

    let init =
      function_
        "comp_words_traverse_init"
        [ Raw_stmt_with_global_named_value (sprintf "%s=0", current_index) ]
    ;;

    let get_current =
      function_
        "comp_words_traverse_get_current"
        [ Call (get_nth, [ Global current_index ]) |> stmt_of_expr ]
    ;;

    let advance =
      function_
        "comp_words_traverse_advance"
        [ Raw_stmt_with_global_named_value
            ((fun v -> sprintf "%s=$((%s + 1))" v v), current_index)
        ]
    ;;

    let is_at_cursor =
      function_
        "comp_words_traverse_is_at_cursor"
        [ Raw_stmt_with_global_named_value
            (sprintf "test \"$%s\" -eq \"$COMP_WORD\"", current_index)
        ]
    ;;

    let is_past_cursor =
      function_
        "comp_words_traverse_is_at_cursor"
        [ Raw_stmt_with_global_named_value
            (sprintf "test \"$%s\" -gt \"$COMP_WORD\"", current_index)
        ]
    ;;

    let advance_if_equals_sign =
      function_
        "comp_words_traverse_advance_if_equals_sign"
        [ If
            ( Raw_with_global_named_value (sprintf "[ \"$(%s)\" == \"=\" ]", current_index)
            , [ Call (advance, []) |> stmt_of_expr ] )
        ]
    ;;

    let all_global_values =
      [ current_index
      ; init
      ; get_current
      ; advance
      ; is_at_cursor
      ; is_past_cursor
      ; advance_if_equals_sign
      ]
    ;;
  end

  let all_global_values = count :: get_nth :: Traverse.all_global_values
end

module Add_reply = struct
  let files =
    function_
      "add_reply_files"
      [ Comment
          "Takes the word under the cursor (just the portion up to the cursor) and \
           completes with files in the current directory."
      ; Raw_stmt "local suggestions"
      ; Raw_stmt "mapfile -t suggestions < <(compgen -A file -- \"$1\")"
      ; For
          ( ("suggestion", Raw "\"${suggestions[@]}\"")
          , [ Comment
                "Insert a space after each suggestion to account for the nospace setting."
            ; Raw_stmt "COMPREPLY+=(\"$suggestion \")"
            ] )
      ]
  ;;

  let fixed =
    function_
      "add_reply_fixed"
      [ Comment
          "Takes the word under the cursor (just the portion up to the cursor) and a \
           space separated list of completion strings."
      ; Raw_stmt "local suggestions"
      ; Raw_stmt "mapfile -t suggestions < <(compgen -W \"$2\" -- \"$1\")"
      ; For
          ( ("suggestion", Raw "\"${suggestions[@]}\"")
          , [ Comment
                "Insert a space after each suggestion to account for the nospace setting."
            ; Raw_stmt "COMPREPLY+=(\"$suggestion \")"
            ] )
      ]
  ;;

  let fixed_no_space_if_ends_with_equals_sign =
    function_
      "add_reply_fixed_no_space_if_ends_with_equals_sign"
      [ Comment
          "Takes the word under the cursor (just the portion up to the cursor) and a \
           space separated list of completion strings, where a completion string ending \
           with a \"=\" indicates that it expects a parameter. Each completion string is \
           added to the commands completion suggestions with no space following \
           completion strings which expect a parameter, and a space following strings \
           which do not."
      ; Raw_stmt "local suggestions"
      ; Raw_stmt "mapfile -t suggestions < <(compgen -W \"$2\" -- \"$1\")"
      ; For
          ( ("suggestion", Raw "\"${suggestions[@]}\"")
          , [ Case
                ( Raw "$suggestion"
                , [ ( "*="
                    , [ Comment
                          "If the argugemnt ends with an equals sign (e.g. \
                           \"--commit=\") then don't add a trailing space. This assumes \
                           that the option `nospace` is being used."
                      ; Raw_stmt "COMPREPLY+=(\"$suggestion\")"
                      ] )
                  ; "*", [ Raw_stmt "COMPREPLY+=(\"$suggestion \")" ]
                  ] )
            ] )
      ]
  ;;

  let all_global_values = [ files; fixed; fixed_no_space_if_ends_with_equals_sign ]
end

module Reentrant_query = struct
  (* This named argument takes an integer index of a reentrant
     query. This will be the parameter of the [Hint.Reentrant_index]
     constructor. *)
  let query_arg_name = Name.of_string_exn "__reentrant-autocompletion-query"

  (* This named argument is passed multiple times - once for each word
     making up the current command. The words are passed to the reentrant
     query so that it can tailor its suggestions to the current state of
     the command line. *)
  let command_line_arg_name = Name.of_string_exn "__reentrant-autocompletion-command-line"

  let wrap_command_line =
    function_
      "reentrant_query_wrap_command_line"
      [ Comment "Wraps its arguments in named arguments for passing to a reentrant query"
      ; For
          ( ("word", Raw "$@")
          , [ Raw
                (sprintf
                   "printf \" --%s=\\\"$word\\\"\""
                   (Name.to_string command_line_arg_name))
              |> stmt_of_expr
            ] )
      ]
  ;;

  let run ~program_exe =
    function_
      "reentrant_query_run"
      [ Comment
          "Takes a space-separated subcommand path, a reentrant query index, and the \
           current word under the cursor (up to the cursor). It invokes the program with \
           the given subcommand path and some special arguments that cause it to emit \
           the result of the requested query. The result is then added to COMPREPLY."
      ; Raw_stmt "local subcommand_path_space_separated=$1"
      ; Raw_stmt "local query_index=$2"
      ; Raw_stmt "local current_word=$3"
      ; Raw_stmt_with_global_named_value
          ( sprintf "local wrapped_command_line=$(eval \"%s $COMP_LINE\")"
          , wrap_command_line )
      ; Raw_stmt
          (sprintf
             "local command=\"%s $subcommand_path_space_separated %s=$query_index \
              $wrapped_command_line"
             program_exe
             (Name.to_string_with_dashes query_arg_name))
      ; Raw_stmt "local suggestions=$(eval $command)"
      ; Raw_stmt
          "mapfile -t suggestions < <(compgen -W \"$suggestions\" -- \"$current_word\")"
      ; For
          ( ("suggestion", Raw "\"${suggestions[@]}\"")
          , [ Comment
                "Insert a space after each suggestion to account for the nospace setting."
            ; Raw_stmt "COMPREPLY+=(\"$suggestion \")"
            ] )
      ]
  ;;
end

module Hint = struct
  type t =
    | File
    | Values of string list
    | Reentrant_index of int

  let add_reply t ~reentrant_query_run ~subcommand_path ~current_word =
    stmt_of_expr
    @@
    match t with
    | File -> Call (Add_reply.files, [ current_word ])
    | Values values ->
      Call (Add_reply.fixed, [ current_word; Raw (String.concat ~sep:" " values) ])
    | Reentrant_index query_index ->
      Call
        ( reentrant_query_run
        , [ Raw (String.concat ~sep:" " subcommand_path)
          ; Raw (string_of_int query_index)
          ; current_word
          ] )
  ;;
end

module Named_arg = struct
  type t =
    { name : Name.t
    ; has_param : bool
    ; hints : Hint.t list
    }

  let to_string { name; has_param; _ } =
    let name_string = Name.to_string_with_dashes name in
    if Name.is_long name && has_param then sprintf "%s=" name_string else name_string
  ;;
end

module Parser_spec = struct
  type t =
    { named_args : Named_arg.t list
    ; positional_args_hints : Hint.t list
    }

  let empty = { named_args = []; positional_args_hints = [] }
end

module Spec = struct
  type t =
    { parser_spec : Parser_spec.t
    ; subcommands : subcommand list
    }

  and subcommand =
    { name : string
    ; spec : t
    }

  let empty = { parser_spec = Parser_spec.empty; subcommands = [] }

  let named_args_sorted { parser_spec = { named_args; _ }; _ } =
    let cmp (arg1 : Named_arg.t) (arg2 : Named_arg.t) =
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
end

module Named_arg_value_completion = struct
  let function_name ~(named_arg : Named_arg.t) ~subcommand_path =
    (* Add the hash of the name to the function name to avoid
       collisions between function names *)
    let hash = Hashtbl.hash (named_arg, subcommand_path) in
    sprintf
      "hash_%d_%s_%s"
      hash
      (String.concat ~sep:"-" subcommand_path)
      (Name.to_string_with_dashes named_arg.name)
  ;;

  (* Generates function for completing the argument to a particular
     named argument to a particular subcommand *)
  let function_ ~(named_arg : Named_arg.t) ~subcommand_path ~reentrant_query_run =
    let hints =
      List.map
        named_arg.hints
        ~f:
          (Hint.add_reply
             ~reentrant_query_run
             ~subcommand_path
             ~current_word:(Raw "$current_word_up_to_cursor"))
    in
    function_
      (function_name ~named_arg ~subcommand_path)
      [ Raw_stmt "local current_word_up_to_cursor=$2"
      ; Call (Comp_words.Traverse.advance_if_equals_sign, []) |> stmt_of_expr
      ; If
          ( Call (Comp_words.Traverse.is_past_cursor, [])
          , [ Return (Global Status.error_word_index_past_cursor) ] )
      ; If
          ( Call (Comp_words.Traverse.is_at_cursor, [])
          , hints @ [ Return (Global Status.done_) ] )
      ]
  ;;
end

module Subcommand_and_positional_arg_completion = struct
  let function_name ~subcommand_path =
    (* Add the hash of the name to the function name to avoid
       collisions between function names *)
    let hash = Hashtbl.hash subcommand_path in
    sprintf "hash_%d_%s" hash (String.concat ~sep:"-" subcommand_path)
  ;;

  let function_ ~(spec : Spec.t) ~subcommand_path =
    (* List of subcommands and named arguments (with dashes) where
       long named arguments with parameters are followed by an equals
       sign *)
    let suggestions =
      List.map spec.subcommands ~f:(fun (subcommand : Spec.subcommand) -> subcommand.name)
      @ (Spec.named_args_sorted spec
         |> List.map ~f:(fun (named_arg : Named_arg.t) ->
           let string_with_dashes = Name.to_string_with_dashes named_arg.name in
           if Name.is_long named_arg.name && named_arg.has_param
           then sprintf "%s=" string_with_dashes
           else string_with_dashes))
      |> String.concat ~sep:" "
    in
    let cases =
      List.map spec.subcommands ~f:(fun (subcommand : Spec.subcommand) ->
        let subcommand_path = subcommand.name :: subcommand_path in
        let completion_function_name = function_name ~subcommand_path in
        let stmts =
          [ Raw_stmt (sprintf "%s \"$1\" \"$2\" \"$3\"" completion_function_name)
          ; Return (Raw "$?")
          ]
        in
        subcommand.name, stmts)
      @ (Spec.named_args_sorted spec
         |> List.filter_map ~f:(fun (named_arg : Named_arg.t) ->
           if named_arg.has_param
           then (
             let completion_function_name =
               Named_arg_value_completion.function_name ~named_arg ~subcommand_path
             in
             let stmts =
               [ Raw_stmt (sprintf "%s \"$1\" \"$2\" \"$3\"" completion_function_name)
               ; Raw_stmt "status=$?"
               ; If (Raw "[ \"$status\" -ne 0 ]", [ Return (Raw "$status") ])
               ]
             in
             Some (Name.to_string_with_dashes named_arg.name, stmts))
           else None))
    in
    function_
      (function_name ~subcommand_path)
      [ Raw_stmt "local current_word_up_to_cursor=$2"
      ; Call (Comp_words.Traverse.advance_if_equals_sign, []) |> stmt_of_expr
      ; While
          ( Raw "true"
          , [ If
                ( Call (Comp_words.Traverse.is_past_cursor, [])
                , [ Return (Global Status.error_word_index_past_cursor) ] )
            ; If_else
                ( [ ( Call (Comp_words.Traverse.is_at_cursor, [])
                    , [ Call
                          ( Add_reply.fixed_no_space_if_ends_with_equals_sign
                          , [ Raw "\"$current_word_up_to_cursor\""
                            ; Raw (sprintf "\"%s\"" suggestions)
                            ] )
                        |> stmt_of_expr
                      ; Return (Global Status.done_)
                      ] )
                  ]
                , [ Raw_stmt "local current_word status"
                  ; Raw_stmt_with_global_named_value
                      (sprintf "current_word=$(%s)", Comp_words.Traverse.get_current)
                  ; Raw_stmt "status=$?"
                  ; If (Raw "[ \"$status\" -ne 0 ]", [ Return (Raw "$status") ])
                  ; Call (Comp_words.Traverse.advance, []) |> stmt_of_expr
                  ; Case (Raw "$current_word", cases)
                  ] )
            ] )
      ]
  ;;
end

module Completion_entry_point = struct
  let function_ ~program_name =
    let completion_root_name =
      Subcommand_and_positional_arg_completion.function_name
        ~subcommand_path:[ program_name ]
    in
    function_
      "completion_entry_point"
      [ Comment
          "Set nospace so that completion doesn't add a space after completing a word. \
           In cases where the space is needed, it will be added manually."
      ; Raw_stmt "compopt -o nospace"
      ; Call (Comp_words.Traverse.init, []) |> stmt_of_expr
      ; If_else
          ( [ ( Raw "[ \"$COMP_CWORD\" == \"0\" ]"
              , [ Call
                    ( Error.print
                    , [ Raw
                          "\"Unexpected $COMP_CWORD value of 0. $COMP_CWORD should be at \
                           least 1 as the shell uses the first word of the command line \
                           to determine which completion script to run.\""
                      ] )
                  |> stmt_of_expr
                ] )
            ; ( Raw_with_global_named_value
                  (sprintf "[ \"$(%s)\" -lt 2 ]", Comp_words.count)
              , [ Call
                    ( Error.print
                    , [ Raw_with_global_named_value
                          ( sprintf
                              "\"Unexpected length of $COMP_WORDS array: $(%s). Its \
                               length should be at least 2 since the first element \
                               should always be the program name, and the second element \
                               will be the first word after the program name, which is \
                               expected to be the empty string if no additional words \
                               have been entered after the program name.\""
                          , Comp_words.count )
                      ] )
                  |> stmt_of_expr
                ] )
            ; ( Raw_with_global_named_value
                  ( (fun comp_words_traverse_get_current ->
                      sprintf
                        "[ \"$(%s)\" != \"%s\" ]"
                        comp_words_traverse_get_current
                        program_name)
                  , Comp_words.Traverse.get_current )
              , [ Call
                    ( Error.print
                    , [ Raw_with_global_named_value
                          ( (fun comp_words_traverse_get_current ->
                              sprintf
                                "\"Completion script found unexpected first word of \
                                 command line: '$(%s)'. This is the completion script \
                                 for the program '%s' and so it's expected that the \
                                 first word of the command line will be '%s'.\""
                                comp_words_traverse_get_current
                                program_name
                                program_name)
                          , Comp_words.Traverse.get_current )
                      ] )
                  |> stmt_of_expr
                ] )
            ]
          , [ Call (Comp_words.Traverse.advance, []) |> stmt_of_expr
            ; Raw_stmt (sprintf "%s \"$1\" \"$2\" \"$3\"" completion_root_name)
            ; Case
                ( Raw "$?"
                , [ Status.done_value, []
                  ; ( Status.error_word_index_past_cursor_value
                    , [ Call
                          ( Error.print
                          , [ Raw
                                "Unexpected error in completion script: Traversed \
                                 command line beyond the current cursor position"
                            ] )
                        |> stmt_of_expr
                      ] )
                  ; ( Status.error_word_index_past_cursor_value
                    , [ Raw_stmt
                          "Unexpected error in completion script: Traversed beyond the \
                           end of the command line"
                      ] )
                  ; ( "*"
                    , [ Call
                          (Error.print, [ Raw "\"Unknown error in completion script\"" ])
                        |> stmt_of_expr
                      ] )
                  ] )
            ] )
      ]
  ;;
end

let rec functions_of_spec (spec : Spec.t) ~subcommand_path ~reentrant_query_run =
  let named_arg_completion_functions =
    List.map spec.parser_spec.named_args ~f:(fun named_arg ->
      Named_arg_value_completion.function_
        ~named_arg
        ~subcommand_path
        ~reentrant_query_run)
  in
  let subcommand_and_positional_arg_completion =
    Subcommand_and_positional_arg_completion.function_ ~spec ~subcommand_path
  in
  let subcommand_completions =
    List.concat_map spec.subcommands ~f:(fun (subcommand : Spec.subcommand) ->
      let subcommand_path = subcommand.name :: subcommand_path in
      functions_of_spec subcommand.spec ~subcommand_path ~reentrant_query_run)
  in
  subcommand_completions
  @ named_arg_completion_functions
  @ [ subcommand_and_positional_arg_completion ]
;;

module Bash = struct
  let rec expr_to_string ~unique_prefix (expr : expr) =
    let global_named_value_to_name_string global_named_value =
      (global_named_value_global_name global_named_value) ~unique_prefix
    in
    match expr with
    | Raw raw -> raw
    | Raw_with_global_named_value (f, global_named_value) ->
      f (global_named_value_to_name_string global_named_value)
    | Call (function_, args) ->
      global_named_value_to_name_string function_
      :: List.map args ~f:(expr_to_string ~unique_prefix)
      |> String.concat ~sep:" "
    | Global global_named_value -> global_named_value_to_name_string global_named_value
  ;;

  let rec of_stmt_with_indent ~unique_prefix = ()
end

let generate spec ~program_name ~program_exe =
  let reentrant_query_run = Reentrant_query.run ~program_exe in
  let static_global_values =
    Status.all_global_values
    @ Error.all_global_values
    @ Comp_words.all_global_values
    @ Add_reply.all_global_values
    @ [ Reentrant_query.wrap_command_line; reentrant_query_run ]
  in
  let completion_functions =
    functions_of_spec spec ~subcommand_path:[ program_name ] ~reentrant_query_run
  in
  ()
;;
