open! Import

let sprintf = Printf.sprintf

module Hint = struct
  type t =
    | File
    | Values of string list
end

module Arg = struct
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

module Spec = struct
  type t =
    { args : Arg.t list
    ; subcommands : subcommand list
    }

  and subcommand =
    { name : string
    ; spec : t
    }

  let empty = { args = []; subcommands = [] }

  let args_sorted { args; _ } =
    let cmp (arg1 : Arg.t) (arg2 : Arg.t) =
      String.compare (Name.to_string arg1.name) (Name.to_string arg2.name)
    in
    let long_args =
      List.filter args ~f:(fun { Arg.name; _ } -> Name.is_long name) |> List.sort ~cmp
    in
    let short_args =
      List.filter args ~f:(fun { Arg.name; _ } -> Name.is_short name) |> List.sort ~cmp
    in
    long_args @ short_args
  ;;

  let generate_code ~prefix ~program_name t =
    let rec generate_code_rec t depth command_line_acc =
      let command_line = List.rev command_line_acc |> String.concat ~sep:" " in
      let complete_args () =
        if List.is_empty t.args
        then sprintf "# command %S has no named arguments" command_line
        else (
          let args_string =
            List.map (args_sorted t) ~f:Arg.to_string |> String.concat ~sep:" "
          in
          sprintf
            "%s_set_reply_no_space_if_ends_with_equals_sign $2 '%s'"
            prefix
            args_string)
      in
      let complete_subcommands () =
        if List.is_empty t.subcommands
        then sprintf "# command %S has no subcommands" command_line
        else (
          let subcommands_string =
            List.map t.subcommands ~f:(fun { name; _ } -> name) |> String.concat ~sep:" "
          in
          sprintf "COMPREPLY=($(compgen -W '%s' -- $2))" subcommands_string)
      in
      let word_index = depth + 1 in
      let indent = String.init (4 * (1 + (2 * depth))) ~f:(Fun.const ' ') in
      let generate_hints () =
        let hint_branches =
          List.filter_map (args_sorted t) ~f:(fun { Arg.name; has_param; hint } ->
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
      ; sprintf "%s        -*) # Arguments of command %S:" indent command_line
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
    generate_code_rec t 0 [ program_name ]
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

let completion_function ~prefix ~program_name spec =
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
    (Spec.generate_code ~prefix ~program_name spec)
    prefix
;;

let register ~program_name ~prefix =
  sprintf "complete -F %s_complete %s" prefix program_name
;;

let generate_bash spec ~program_name =
  (* Add a random prefix to all global names so we don't collide with other global shell functions. *)
  Random.self_init ();
  let prefix = sprintf "_%d" (Random.int32 Int32.max_int |> Int32.to_int) in
  String.concat
    ~sep:"\n"
    [ preamble ~program_name ~prefix
    ; completion_function ~prefix ~program_name spec
    ; register ~prefix ~program_name
    ]
;;
