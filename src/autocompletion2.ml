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

and function_call =
  { function_ : global_named_value
  ; args : value list
  }

and string_with_global_named_value =
  { string_of_name : string -> string
  ; global_named_value : global_named_value
  }

and value =
  | Literal of string
  | Literal_with_global_named_value of string_with_global_named_value
  | Global of global_named_value
  | Call_output of function_call

and cond =
  | True
  | Call of function_call
  | Test_raw_cond of string
  | Test_raw_cond_of_string_with_global_named_value of string_with_global_named_value

and block = stmt list

and conditional_block =
  { cond : cond
  ; body : block
  }

and if_ =
  { if_ : conditional_block
  ; elifs : conditional_block list
  ; else_ : block option
  }

and case_pattern =
  { pattern : string
  ; case_body : block
  }

and for_ =
  { var : string
  ; seq : value
  ; for_body : block
  }

and stmt =
  | Raw of string
  | Raw_with_global_named_value of string_with_global_named_value
  | Cond of cond
  | If of if_
  | Case of
      { value : value
      ; patterns : case_pattern list
      }
  | While of conditional_block
  | For of for_
  | Return of value
  | Comment of string

module Global_named_value = struct
  let global_name = function
    | Global_variable { name; _ } | Function { name; _ } -> name
  ;;

  let global_name_with_prefix ~unique_prefix t = (global_name t) ~unique_prefix

  let string_with_global_named_value_to_string
    ~unique_prefix
    { string_of_name; global_named_value }
    =
    string_of_name (global_name_with_prefix ~unique_prefix global_named_value)
  ;;

  let global_variable name initial_value =
    Global_variable { name = name_with_prefix name; initial_value }
  ;;

  let function_ name body = Function { name = name_with_prefix name; body }
end

module Value = struct
  let literal s = Literal s

  let literal_with_global_named_value ~f global_named_value =
    Literal_with_global_named_value { string_of_name = f; global_named_value }
  ;;

  let global global_named_value = Global global_named_value
  let call_output function_ args = Call_output { function_; args }
end

module Cond = struct
  let true_ = True
  let call function_ args = Call { function_; args }
  let test_raw s = Test_raw_cond s

  let test_raw_of_string_with_global_named_value ~f global_named_value =
    Test_raw_cond_of_string_with_global_named_value
      { string_of_name = f; global_named_value }
  ;;
end

module Stmt = struct
  let raw s = Raw s

  let raw_with_global_named_value ~f global_named_value =
    Raw_with_global_named_value { string_of_name = f; global_named_value }
  ;;

  let call function_ args = Cond (Cond.call function_ args)

  let test_raw_cond_of_string_with_global_named_value ~f global_named_value =
    Cond (Cond.test_raw_of_string_with_global_named_value ~f global_named_value)
  ;;

  let if_ ?elifs ?else_ cond body =
    let elifs =
      Option.map elifs ~f:(List.map ~f:(fun (cond, body) -> { cond; body }))
      |> Option.value ~default:[]
    in
    If { if_ = { cond; body }; elifs; else_ }
  ;;

  let case value patterns =
    let patterns =
      List.map patterns ~f:(fun (pattern, case_body) -> { pattern; case_body })
    in
    Case { value; patterns }
  ;;

  let while_ cond body = While { cond; body }
  let for_ var seq for_body = For { var; seq; for_body }
  let return s = Return s
  let comment s = Comment s
end

module Status = struct
  open Global_named_value

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
    let open Stmt in
    function_
      "status_is_error"
      [ test_raw_cond_of_string_with_global_named_value
          ~f:(sprintf "\"$1\" -gt \"$%s\"")
          done_
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
    function_ "error_print" [ raw "echo $1 > /dev/stderr" ]
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
          (Cond.test_raw_of_string_with_global_named_value
             ~f:(sprintf "\"$i\" -ge \"$(%s)\"")
             count)
          [ return (Value.global Status.error_word_out_of_bounds) ]
      ]
  ;;

  module Traverse = struct
    let current_index = global_variable "COMP_WORDS_CURRENT_INDEX" "0"

    let init =
      let open Stmt in
      function_
        "comp_words_traverse_init"
        [ raw_with_global_named_value ~f:(sprintf "%s=0") current_index ]
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
        [ raw_with_global_named_value
            ~f:(fun v -> sprintf "%s=$((%s + 1))" v v)
            current_index
        ]
    ;;

    let is_at_cursor =
      let open Stmt in
      function_
        "comp_words_traverse_is_at_cursor"
        [ raw_with_global_named_value
            ~f:(sprintf "test \"$%s\" -eq \"$COMP_WORD\"")
            current_index
        ]
    ;;

    let is_past_cursor =
      let open Stmt in
      function_
        "comp_words_traverse_is_at_cursor"
        [ raw_with_global_named_value
            ~f:(sprintf "test \"$%s\" -gt \"$COMP_WORD\"")
            current_index
        ]
    ;;

    let advance_if_equals_sign =
      let open Stmt in
      function_
        "comp_words_traverse_advance_if_equals_sign"
        [ if_
            (Cond.test_raw_of_string_with_global_named_value
               ~f:(sprintf "\"$(%s)\" == \"=\"")
               current_index)
            [ call advance [] ]
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
  open Global_named_value

  let files =
    let open Stmt in
    function_
      "add_reply_files"
      [ comment
          "Takes the word under the cursor (just the portion up to the cursor) and \
           completes with files in the current directory."
      ; raw "local suggestions"
      ; raw "mapfile -t suggestions < <(compgen -A file -- \"$1\")"
      ; for_
          "suggestion"
          (Value.literal "\"${suggestions[@]}\"")
          [ comment
              "Insert a space after each suggestion to account for the nospace setting."
          ; raw "COMPREPLY+=(\"$suggestion \")"
          ]
      ]
  ;;

  let fixed =
    let open Stmt in
    function_
      "add_reply_fixed"
      [ comment
          "Takes the word under the cursor (just the portion up to the cursor) and a \
           space separated list of completion strings."
      ; raw "local suggestions"
      ; raw "mapfile -t suggestions < <(compgen -W \"$2\" -- \"$1\")"
      ; for_
          "suggestion"
          (Value.literal "\"${suggestions[@]}\"")
          [ comment
              "Insert a space after each suggestion to account for the nospace setting."
          ; raw "COMPREPLY+=(\"$suggestion \")"
          ]
      ]
  ;;

  let fixed_no_space_if_ends_with_equals_sign =
    let open Stmt in
    function_
      "add_reply_fixed_no_space_if_ends_with_equals_sign"
      [ comment
          "Takes the word under the cursor (just the portion up to the cursor) and a \
           space separated list of completion strings, where a completion string ending \
           with a \"=\" indicates that it expects a parameter. Each completion string is \
           added to the commands completion suggestions with no space following \
           completion strings which expect a parameter, and a space following strings \
           which do not."
      ; raw "local suggestions"
      ; raw "mapfile -t suggestions < <(compgen -W \"$2\" -- \"$1\")"
      ; for_
          "suggestion"
          (Value.literal "\"${suggestions[@]}\"")
          [ case
              (Value.literal "$suggestion")
              [ ( "*="
                , [ comment
                      "If the argugemnt ends with an equals sign (e.g. \"--commit=\") \
                       then don't add a trailing space. This assumes that the option \
                       `nospace` is being used."
                  ; raw "COMPREPLY+=(\"$suggestion\")"
                  ] )
              ; "*", [ raw "COMPREPLY+=(\"$suggestion \")" ]
              ]
          ]
      ]
  ;;

  let all_global_values = [ files; fixed; fixed_no_space_if_ends_with_equals_sign ]
end

module Reentrant_query = struct
  open Global_named_value

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
    let open Stmt in
    function_
      "reentrant_query_wrap_command_line"
      [ comment "Wraps its arguments in named arguments for passing to a reentrant query"
      ; for_
          "word"
          (Value.literal "$@")
          [ raw
              (sprintf
                 "printf \" --%s=\\\"$word\\\"\""
                 (Name.to_string command_line_arg_name))
          ]
      ]
  ;;

  let run ~program_exe =
    let open Stmt in
    function_
      "reentrant_query_run"
      [ comment
          "Takes a space-separated subcommand path, a reentrant query index, and the \
           current word under the cursor (up to the cursor). It invokes the program with \
           the given subcommand path and some special arguments that cause it to emit \
           the result of the requested query. The result is then added to COMPREPLY."
      ; raw "local subcommand_path_space_separated=$1"
      ; raw "local query_index=$2"
      ; raw "local current_word=$3"
      ; raw_with_global_named_value
          ~f:(sprintf "local wrapped_command_line=$(eval \"%s $COMP_LINE\")")
          wrap_command_line
      ; raw
          (sprintf
             "local command=\"%s $subcommand_path_space_separated %s=$query_index \
              $wrapped_command_line"
             program_exe
             (Name.to_string_with_dashes query_arg_name))
      ; raw "local suggestions=$(eval $command)"
      ; raw "mapfile -t suggestions < <(compgen -W \"$suggestions\" -- \"$current_word\")"
      ; for_
          "suggestion"
          (Value.literal "\"${suggestions[@]}\"")
          [ comment
              "Insert a space after each suggestion to account for the nospace setting."
          ; raw "COMPREPLY+=(\"$suggestion \")"
          ]
      ]
  ;;
end

module Hint = struct
  open Global_named_value

  type t =
    | File
    | Values of string list
    | Reentrant_index of int

  let add_reply t ~reentrant_query_run ~subcommand_path ~current_word =
    let open Stmt in
    match t with
    | File -> call Add_reply.files [ current_word ]
    | Values values ->
      call Add_reply.fixed [ current_word; Value.literal (String.concat ~sep:" " values) ]
    | Reentrant_index query_index ->
      call
        reentrant_query_run
        [ Value.literal (String.concat ~sep:" " subcommand_path)
        ; Value.literal (string_of_int query_index)
        ; current_word
        ]
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
  open Global_named_value

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
    let open Stmt in
    let hints =
      List.map
        named_arg.hints
        ~f:
          (Hint.add_reply
             ~reentrant_query_run
             ~subcommand_path
             ~current_word:(Value.literal "$current_word_up_to_cursor"))
    in
    function_
      (function_name ~named_arg ~subcommand_path)
      [ raw "local current_word_up_to_cursor=$2"
      ; call Comp_words.Traverse.advance_if_equals_sign []
      ; if_
          (Cond.call Comp_words.Traverse.is_past_cursor [])
          [ return (Value.global Status.error_word_index_past_cursor) ]
      ; if_
          (Cond.call Comp_words.Traverse.is_at_cursor [])
          (hints @ [ return (Value.global Status.done_) ])
      ]
  ;;
end

module Subcommand_and_positional_arg_completion = struct
  open Global_named_value

  let function_name ~subcommand_path =
    (* Add the hash of the name to the function name to avoid
       collisions between function names *)
    let hash = Hashtbl.hash subcommand_path in
    sprintf "hash_%d_%s" hash (String.concat ~sep:"-" subcommand_path)
  ;;

  let function_ ~(spec : Spec.t) ~subcommand_path =
    let open Stmt in
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
          [ raw (sprintf "%s \"$1\" \"$2\" \"$3\"" completion_function_name)
          ; Return (Value.literal "$?")
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
               [ raw (sprintf "%s \"$1\" \"$2\" \"$3\"" completion_function_name)
               ; raw "status=$?"
               ; if_
                   (Cond.test_raw "\"$status\" -ne 0")
                   [ return (Value.literal "$status") ]
               ]
             in
             Some (Name.to_string_with_dashes named_arg.name, stmts))
           else None))
    in
    function_
      (function_name ~subcommand_path)
      [ raw "local current_word_up_to_cursor=$2"
      ; call Comp_words.Traverse.advance_if_equals_sign []
      ; while_
          Cond.true_
          [ if_
              (Cond.call Comp_words.Traverse.is_past_cursor [])
              [ return (Value.global Status.error_word_index_past_cursor) ]
          ; if_
              (Cond.call Comp_words.Traverse.is_at_cursor [])
              [ call
                  Add_reply.fixed_no_space_if_ends_with_equals_sign
                  [ Value.literal "\"$current_word_up_to_cursor\""
                  ; Value.literal suggestions
                  ]
              ; return (Value.global Status.done_)
              ]
              ~else_:
                [ raw "local current_word status"
                ; raw_with_global_named_value
                    ~f:(sprintf "current_word=$(%s)")
                    Comp_words.Traverse.get_current
                ; raw "status=$?"
                ; if_
                    (Cond.test_raw "\"$status\" -ne 0")
                    [ return (Value.literal "$status") ]
                ; call Comp_words.Traverse.advance []
                ; case (Value.literal "$current_word") cases
                ]
          ]
      ]
  ;;
end

module Completion_entry_point = struct
  open Global_named_value

  let function_ ~program_name =
    let open Stmt in
    let completion_root_name =
      Subcommand_and_positional_arg_completion.function_name
        ~subcommand_path:[ program_name ]
    in
    function_
      "completion_entry_point"
      [ comment
          "Set nospace so that completion doesn't add a space after completing a word. \
           In cases where the space is needed, it will be added manually."
      ; raw "compopt -o nospace"
      ; call Comp_words.Traverse.init []
      ; if_
          (Cond.test_raw "\"$COMP_CWORD\" == \"0\"")
          [ call
              Error.print
              [ Value.literal
                  "\"Unexpected $COMP_CWORD value of 0. $COMP_CWORD should be at least 1 \
                   as the shell uses the first word of the command line to determine \
                   which completion script to run.\""
              ]
          ]
          ~elifs:
            [ ( Cond.test_raw_of_string_with_global_named_value
                  ~f:(sprintf "\"$(%s)\" -lt 2")
                  Comp_words.count
              , [ call
                    Error.print
                    [ Value.literal
                        "\"Unexpected length of $COMP_WORDS array: $(%s). Its length \
                         should be at least 2 since the first element should always be \
                         the program name, and the second element will be the first word \
                         after the program name, which is expected to be the empty \
                         string if no additional words have been entered after the \
                         program name.\""
                    ]
                ] )
            ; ( Cond.test_raw_of_string_with_global_named_value
                  ~f:(fun comp_words_traverse_get_current ->
                    sprintf
                      "\"$(%s)\" != \"%s\""
                      comp_words_traverse_get_current
                      program_name)
                  Comp_words.Traverse.get_current
              , [ call
                    Error.print
                    [ Value.literal_with_global_named_value
                        ~f:(fun comp_words_traverse_get_current ->
                          sprintf
                            "\"Completion script found unexpected first word of command \
                             line: '$(%s)'. This is the completion script for the \
                             program '%s' and so it's expected that the first word of \
                             the command line will be '%s'.\""
                            comp_words_traverse_get_current
                            program_name
                            program_name)
                        Comp_words.Traverse.get_current
                    ]
                ] )
            ]
          ~else_:
            [ call Comp_words.Traverse.advance []
            ; raw (sprintf "%s \"$1\" \"$2\" \"$3\"" completion_root_name)
            ; case
                (Value.literal "$?")
                [ Status.done_value, []
                ; ( Status.error_word_index_past_cursor_value
                  , [ call
                        Error.print
                        [ Value.literal
                            "Unexpected error in completion script: Traversed command \
                             line beyond the current cursor position"
                        ]
                    ] )
                ; ( Status.error_word_index_past_cursor_value
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
                        [ Value.literal "\"Unknown error in completion script\"" ]
                    ] )
                ]
            ]
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
  let rec value_to_string ~unique_prefix = function
    | Literal s -> s
    | Literal_with_global_named_value string_with_global_named_value ->
      Global_named_value.string_with_global_named_value_to_string
        ~unique_prefix
        string_with_global_named_value
    | Global global_named_value ->
      sprintf
        "\"$%s\""
        (Global_named_value.global_name_with_prefix ~unique_prefix global_named_value)
    | Call_output function_call ->
      sprintf "\"$(%s)\"" (function_call_to_string ~unique_prefix function_call)

  and function_call_to_string ~unique_prefix { function_; args } =
    let function_name =
      Global_named_value.global_name_with_prefix ~unique_prefix function_
    in
    let args_strings = List.map args ~f:(value_to_string ~unique_prefix) in
    String.concat ~sep:" " (function_name :: args_strings)
  ;;

  let cond_to_string ~unique_prefix = function
    | True -> "true"
    | Call function_call -> function_call_to_string ~unique_prefix function_call
    | Test_raw_cond s -> sprintf "[ %s ]" s
    | Test_raw_cond_of_string_with_global_named_value string_with_global_named_value ->
      let s =
        Global_named_value.string_with_global_named_value_to_string
          ~unique_prefix
          string_with_global_named_value
      in
      sprintf "[ %s ]" s
  ;;

  type line =
    { indent : int
    ; text : string
    }

  let split_string_on_with_max_line_length s ~sep ~max_line_length =
    let words = String.split_on_char ~sep s in
    List.fold_left words ~init:[] ~f:(fun acc word ->
      let word_length = String.length word in
      match acc with
      | [] -> [ [ word ], word_length ]
      | (last_line, last_line_length) :: xs ->
        let new_last_line_length = last_line_length + word_length + 1 in
        if new_last_line_length > max_line_length
        then ([ word ], word_length) :: acc
        else (word :: last_line, last_line_length) :: xs)
    |> List.rev
    |> List.map ~f:(fun (line, _length) ->
      List.rev line |> String.concat ~sep:(String.make 1 sep))
  ;;

  let rec stmt_to_lines_with_indent ~unique_prefix ~indent = function
    | Raw text -> [ { indent; text } ]
    | Raw_with_global_named_value string_with_global_named_value ->
      let text =
        Global_named_value.string_with_global_named_value_to_string
          ~unique_prefix
          string_with_global_named_value
      in
      [ { text; indent } ]
    | Cond c ->
      let text = cond_to_string ~unique_prefix c in
      [ { text; indent } ]
    | If { if_; elifs; else_ } ->
      ({ indent; text = sprintf "if %s; then" (cond_to_string ~unique_prefix if_.cond) }
       :: List.concat_map
            if_.body
            ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:(indent + 1)))
      @ List.concat_map elifs ~f:(fun { cond; body } ->
        { indent; text = sprintf "elif %s; then" (cond_to_string ~unique_prefix cond) }
        :: List.concat_map
             body
             ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:(indent + 1)))
      @ (match else_ with
         | None -> []
         | Some stmts ->
           { indent; text = "else" }
           :: List.concat_map
                stmts
                ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:(indent + 1)))
      @ [ { indent; text = "fi" } ]
    | Case { value; patterns } ->
      ({ indent; text = sprintf "case %s in" (value_to_string ~unique_prefix value) }
       :: List.concat_map patterns ~f:(fun { pattern; case_body } ->
         ({ indent = indent + 1; text = sprintf "%s)" pattern }
          :: List.concat_map
               case_body
               ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:(indent + 2)))
         @ [ { indent = indent + 2; text = ";;" } ]))
      @ [ { indent; text = "esac" } ]
    | While { cond; body } ->
      ({ indent; text = sprintf "while %s; do" (cond_to_string ~unique_prefix cond) }
       :: List.concat_map
            body
            ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:(indent + 1)))
      @ [ { indent; text = "done" } ]
    | For { var; seq; for_body } ->
      ({ indent
       ; text = sprintf "for %s in %s; do" var (value_to_string ~unique_prefix seq)
       }
       :: List.concat_map
            for_body
            ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:(indent + 1)))
      @ [ { indent; text = "done" } ]
    | Return value ->
      [ { indent; text = sprintf "return %s" (value_to_string ~unique_prefix value) } ]
    | Comment comment ->
      let comment_line_prefix = "# " in
      let max_line_length =
        Int.max (80 - String.length comment_line_prefix - (indent * 4)) 20
      in
      let lines =
        split_string_on_with_max_line_length ~sep:' ' ~max_line_length comment
      in
      List.map lines ~f:(fun line ->
        { indent; text = String.cat comment_line_prefix line })
  ;;

  let global_named_value_to_lines ~unique_prefix = function
    | Global_variable { name; initial_value } ->
      let name = name ~unique_prefix in
      [ { indent = 0; text = sprintf "%s=%s" name initial_value } ]
    | Function { name; body } ->
      let name = name ~unique_prefix in
      ({ indent = 0; text = sprintf "%s() {" name }
       :: List.concat_map body ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:1))
      @ [ { indent = 0; text = "}" } ]
  ;;

  let lines_to_string lines =
    let indent_size = 4 in
    List.map lines ~f:(fun { indent; text } ->
      let indent_string = String.make (indent * indent_size) ' ' in
      String.cat indent_string text)
    |> String.concat ~sep:"\n"
  ;;

  let global_named_value_to_string ~unique_prefix global_named_value =
    global_named_value_to_lines ~unique_prefix global_named_value |> lines_to_string
  ;;

  let stmt_to_string ~unique_prefix stmt =
    stmt_to_lines_with_indent ~indent:0 ~unique_prefix stmt |> lines_to_string
  ;;
end

let make_unique_prefix ~program_name =
  Random.self_init ();
  sprintf
    "__climate_complete_%s_%d"
    program_name
    (Random.int32 Int32.max_int |> Int32.to_int)
;;

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
  let all_functions = static_global_values @ completion_functions in
  let unique_prefix = make_unique_prefix ~program_name in
  let globals =
    List.map all_functions ~f:(Bash.global_named_value_to_string ~unique_prefix)
  in
  let last_line =
    Stmt.raw_with_global_named_value
      (Completion_entry_point.function_ ~program_name)
      ~f:(fun complete_entry -> sprintf "complete -F %s %s" complete_entry program_name)
    |> Bash.stmt_to_string ~unique_prefix
  in
  globals @ [ last_line ] |> String.concat ~sep:"\n\n"
;;
