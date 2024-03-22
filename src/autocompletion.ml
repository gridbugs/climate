open! Import

let sprintf = Printf.sprintf

let reentrant_autocompletion_query_name =
  Name.of_string_exn "__reentrant-autocompletion-query"
;;

let reentrant_autocompletion_command_line_name =
  Name.of_string_exn "__reentrant-autocompletion-command-line"
;;

module Hint = struct
  type t =
    | File
    | Values of string list
    | Reentrant_index of int
end

module Named_arg = struct
  type t =
    { name : Name.t
    ; has_param : bool
    ; hint : Hint.t option
    }

  let to_string { name; has_param; _ } =
    let name_string = Name.to_string_with_dashes name in
    if Name.is_long name && has_param then sprintf "%s=" name_string else name_string
  ;;
end

module Parser_spec = struct
  type t =
    { named_args : Named_arg.t list
    ; positional_args_hint :
        Hint.t option (* TODO allow different hints for different positional args *)
    }

  let empty = { named_args = []; positional_args_hint = None }
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

  let generate_code ~prefix ~program_name ~program_exe t =
    let rec generate_code_rec t depth command_line_acc =
      let command_line =
        program_name :: List.rev command_line_acc |> String.concat ~sep:" "
      in
      let word_index = depth + 1 in
      let indent = String.init (4 * (1 + (2 * depth))) ~f:(Fun.const ' ') in
      let complete_args () =
        if List.is_empty t.parser_spec.named_args
        then sprintf "# command %S has no named arguments" command_line
        else (
          let args_string =
            List.map (named_args_sorted t) ~f:Named_arg.to_string
            |> String.concat ~sep:" "
          in
          sprintf
            "%s_set_reply_no_space_if_ends_with_equals_sign $2 '%s'"
            prefix
            args_string)
      in
      let generate_hints_positional () =
        let commands =
          match t.parser_spec.positional_args_hint with
          | None -> [ ": # no hint " ]
          | Some hint ->
            (match hint with
             | File -> [ sprintf "%s_set_reply_files" prefix ]
             | Values values ->
               [ sprintf
                   "COMPREPLY=($(compgen -W '%s' -- $2))"
                   (String.concat ~sep:" " values)
               ]
             | Reentrant_index i ->
               let this_subcommand = String.concat ~sep:" " (List.rev command_line_acc) in
               [ sprintf
                   "local suggestions=$(%s %s %s=%d %s=\"$COMP_LINE\")"
                   program_exe
                   this_subcommand
                   (Name.to_string_with_dashes reentrant_autocompletion_query_name)
                   i
                   (Name.to_string_with_dashes reentrant_autocompletion_command_line_name)
               ; "COMPREPLY=($(compgen -W \"$suggestions\" -- $2))"
               ])
        in
        List.map commands ~f:(fun command -> sprintf "%s            %s" indent command)
        |> String.concat ~sep:"\n"
      in
      let complete_subcommands () =
        if List.is_empty t.subcommands
        then generate_hints_positional ()
        else (
          let subcommands_string =
            List.map t.subcommands ~f:(fun { name; _ } -> name) |> String.concat ~sep:" "
          in
          sprintf "COMPREPLY=($(compgen -W '%s' -- $2))" subcommands_string)
      in
      let generate_hints () =
        let hint_branches =
          List.filter_map
            (named_args_sorted t)
            ~f:(fun { Named_arg.name; has_param; hint } ->
              if has_param
              then
                Option.map hint ~f:(fun hint ->
                  let commands =
                    match hint with
                    | File -> [ sprintf "%s_set_reply_files" prefix ]
                    | Values values ->
                      [ sprintf
                          "COMPREPLY=($(compgen -W '%s' -- $2))"
                          (String.concat ~sep:" " values)
                      ]
                    | Reentrant_index i ->
                      let this_subcommand =
                        String.concat ~sep:" " (List.rev command_line_acc)
                      in
                      [ sprintf
                          "local suggestions=$(%s %s %s=%d %s=\"$COMP_LINE\")"
                          program_exe
                          this_subcommand
                          (Name.to_string_with_dashes reentrant_autocompletion_query_name)
                          i
                          (Name.to_string_with_dashes
                             reentrant_autocompletion_command_line_name)
                      ; "COMPREPLY=($(compgen -W \"$suggestions\" -- $2))"
                      ]
                  in
                  (sprintf "%s        %s)" indent (Name.to_string_with_dashes name)
                   :: List.map commands ~f:(fun command ->
                     sprintf "%s              %s" indent command))
                  @ [ sprintf "%s              ;;" indent ]
                  |> String.concat ~sep:"\n")
              else None)
        in
        [ sprintf
            "%s        # If the current word is preceeded by an \"=\" sign then skip to \
             the previous word."
            indent
        ; sprintf "%s        local last_arg=$3" indent
        ; sprintf "%s        if [ \"$last_arg\" == \"=\" ]" indent
        ; sprintf "%s        then" indent
        ; sprintf "%s            last_arg=${COMP_WORDS[$((COMP_CWORD-2))]}" indent
        ; sprintf "%s        fi" indent
        ; sprintf "%s        case \"$last_arg\" in" indent
        ]
        @ hint_branches
        @ [ sprintf "%s        *)" indent
          ; sprintf "%s            %s" indent (complete_args ())
          ; sprintf "%s            ;;" indent
          ; sprintf "%s    esac" indent
          ]
        |> String.concat ~sep:"\n"
      in
      let generate_subcommands () =
        let subcommand_branches =
          List.map t.subcommands ~f:(fun { name; spec } ->
            [ sprintf "%s        %s)" indent name
            ; generate_code_rec spec (depth + 1) (name :: command_line_acc)
            ; sprintf "%s        ;;" indent
            ]
            |> String.concat ~sep:"\n")
        in
        (sprintf "%s    case \"${COMP_WORDS[%d]}\" in" indent word_index
         :: subcommand_branches)
        @ [ sprintf "%s        *)" indent
          ; generate_hints ()
          ; sprintf "%s            ;;" indent
          ; sprintf "%s    esac" indent
          ]
        |> String.concat ~sep:"\n"
      in
      [ sprintf "%s# Begin handling of command %S" indent command_line
      ; sprintf "%sif [ \"$COMP_CWORD\" == \"%d\" ]" indent word_index
      ; sprintf "%sthen" indent
      ; sprintf "%s    case $2 in" indent
      ; sprintf "%s        -*) # Named_arguments of command %S:" indent command_line
      ; sprintf "%s            %s" indent (complete_args ())
      ; sprintf "%s            ;;" indent
      ; sprintf "%s        *) # Subcommands of command %S:" indent command_line
      ; sprintf "%s            %s" indent (complete_subcommands ())
      ; sprintf "%s            ;;" indent
      ; sprintf "%s    esac" indent
      ; sprintf "%selse" indent
      ; generate_subcommands ()
      ; sprintf "%sfi" indent
      ; sprintf "%s# End handling of command %S" indent command_line
      ]
      |> String.concat ~sep:"\n"
    in
    generate_code_rec t 0 []
  ;;
end

let preamble ~program_name ~prefix =
  sprintf
    {|# Autocompletion script for %S generated by climate.

# Takes the word under the cursor (just the portion up to the cursor) and a
# space separated list of completion strings, where a completion string ending
# with a "=" indicates that it expects a parameter. Each completion string is
# added to the commands completion suggestions with no space following completion
# strings which expect a parameter, and a space following strings which do not.
%s_set_reply_no_space_if_ends_with_equals_sign() {
    compopt -o nospace
    local suggestions=($(compgen -W "$2" -- $1))
    for suggestion in "${suggestions[@]}"; do
        case $suggestion in
            *=)
                COMPREPLY+=($suggestion)
                ;;
            *)
                COMPREPLY+=("$suggestion ")
                ;;
        esac
    done
}

# Takes the word under the cursor and completes with files in the current
# directory with some nicer formatting (e.g. directories have a slash added to
# the end of their names).
%s_set_reply_files() {
    compopt -o filenames
    COMPREPLY=($(compgen -A file -- $1))
}
|}
    program_name
    prefix
    prefix
;;

let completion_function ~prefix ~program_name ~program_exe spec =
  sprintf
    {|%s_complete() {
    if [ "$COMP_CWORD" == "0" ]; then
        # should never happen
        return
    fi

%s

    # If there are no completions set, fall back to completing local filenames
    if [ "${#COMPREPLY[@]}" == "0" ]; then
        %s_set_reply_files $2
    fi
}
|}
    prefix
    (Spec.generate_code ~prefix ~program_name ~program_exe spec)
    prefix
;;

let register ~program_name ~prefix =
  sprintf "complete -F %s_complete %s" prefix program_name
;;

let generate_bash spec ~program_name ~program_exe =
  (* Add a random prefix to all global names so we don't collide with other global shell functions. *)
  Random.self_init ();
  let prefix =
    sprintf "_climate_autocompletion_%d" (Random.int32 Int32.max_int |> Int32.to_int)
  in
  String.concat
    ~sep:"\n"
    [ preamble ~program_name ~prefix
    ; completion_function ~prefix ~program_name ~program_exe spec
    ; register ~prefix ~program_name
    ]
;;
