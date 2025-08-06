open! Import
module Value = Command_doc_spec.Value

module Style = struct
  type t =
    { program_doc : Ansi_style.t
    ; usage : Ansi_style.t
    ; arg_name : Ansi_style.t
    ; arg_doc : Ansi_style.t
    ; section_heading : Ansi_style.t
    ; error : Ansi_style.t
    }

  let plain =
    { program_doc = Ansi_style.default
    ; usage = Ansi_style.default
    ; arg_name = Ansi_style.default
    ; arg_doc = Ansi_style.default
    ; section_heading = Ansi_style.default
    ; error = Ansi_style.default
    }
  ;;

  let default =
    { plain with
      arg_name = { Ansi_style.default with color = Some `Magenta; bold = true }
    ; section_heading = { Ansi_style.default with color = Some `Blue; bold = true }
    ; error = { Ansi_style.default with color = Some `Red; bold = true }
    }
  ;;
end

let pp_print_elipsis ppf () = Format.pp_print_string ppf "…"

let rec pp_print_newlines ppf = function
  | n when n <= 0 -> ()
  | n ->
    Format.pp_print_newline ppf ();
    pp_print_newlines ppf (n - 1)
;;

let sep = ", "
let pp_sep ppf () = Format.pp_print_string ppf sep

let rec pp_print_spaces ppf = function
  | n when n <= 0 -> ()
  | n ->
    Format.pp_print_string ppf " ";
    pp_print_spaces ppf (n - 1)
;;

module Print = struct
  module Names = struct
    (* An entry possibly has several names in the case of command aliases or
       options with short and long variants. Names are split into left and
       right names. The two groups of names will be printed separated by a
       comma, and padded such that all right names are left-aligned with each
       other. The goal is to allow arguments to be printed like:
       -h, --help        Print this message.
       -v, --verbose...  Use verbose output.
       ____--foo         Argument with no short name.

       In the above, underscores are used instead of spaces as ocamlformat
       would otherwise remove the spaces. The point is that "--foo" is
       left-aligned with "--verbose" and "--help".
    *)
    type t =
      { left : string list
      ; right : string list
      }

    let empty = { left = []; right = [] }
    let is_empty { left; right } = List.is_empty left && List.is_empty right
    let of_right right = { left = []; right }

    let left_string { left; _ } =
      let ppf = Format.str_formatter in
      Format.pp_print_list ~pp_sep Format.pp_print_string ppf left;
      Format.flush_str_formatter ()
    ;;

    let pp_padded ppf ~at_least_one_left_name ~right_names_left_padding t =
      let left_string = left_string t in
      let remaining = right_names_left_padding - String.length left_string in
      pp_print_spaces ppf remaining;
      Format.pp_print_string ppf left_string;
      if not (List.is_empty t.right)
      then (
        if at_least_one_left_name
        then
          if List.is_empty t.left
          then pp_print_spaces ppf (String.length sep)
          else pp_sep ppf ();
        Format.pp_print_list ~pp_sep Format.pp_print_string ppf t.right)
    ;;
  end

  module Entry = struct
    type t =
      { names : Names.t
      ; value : Value.t option
      ; doc : string option
      ; repeated : bool
      }

    let indent = 2

    let pp_names_value_padded ppf ~at_least_one_left_name ~right_names_left_padding t =
      if not (Names.is_empty t.names)
      then Names.pp_padded ppf ~at_least_one_left_name ~right_names_left_padding t.names;
      Option.iter t.value ~f:(fun value ->
        if not (Names.is_empty t.names) then pp_print_spaces ppf 1;
        Value.pp ~format_name:Fun.id ppf value;
        if t.repeated then pp_print_elipsis ppf ())
    ;;

    let names_value_padded_to_string ~at_least_one_left_name ~right_names_left_padding t =
      let ppf = Format.str_formatter in
      pp_names_value_padded ppf ~at_least_one_left_name ~right_names_left_padding t;
      Format.flush_str_formatter ()
    ;;

    let pp_padded
      (style : Style.t)
      ppf
      ~at_least_one_left_name
      ~right_names_left_padding
      ~doc_left_padding
      t
      =
      pp_print_spaces ppf indent;
      let names_value_string =
        names_value_padded_to_string ~at_least_one_left_name ~right_names_left_padding t
      in
      Ansi_style.pp_with_style style.arg_name ppf ~f:(fun ppf ->
        Format.pp_print_string ppf names_value_string);
      pp_print_spaces ppf 2;
      Option.iter t.doc ~f:(fun doc ->
        let padding = doc_left_padding - String.length names_value_string in
        pp_print_spaces ppf padding;
        Ansi_style.pp_with_style style.arg_doc ppf ~f:(fun ppf ->
          Format.pp_print_string ppf doc));
      Format.pp_print_newline ppf ()
    ;;
  end

  module Section = struct
    type t =
      { section_heading : string
      ; entries : Entry.t list
      }

    let max_left_length t =
      List.map t.entries ~f:(fun { Entry.names; _ } ->
        Names.left_string names |> String.length)
      |> List.max
      |> Option.value ~default:0
    ;;

    let max_name_length ~at_least_one_left_name ~right_names_left_padding t =
      List.map t.entries ~f:(fun entry ->
        Entry.names_value_padded_to_string
          ~at_least_one_left_name
          ~right_names_left_padding
          entry
        |> String.length)
      |> List.max
      |> Option.value ~default:0
    ;;

    let pp (style : Style.t) ppf t =
      if List.is_empty t.entries
      then ()
      else (
        let at_least_one_left_name =
          List.exists t.entries ~f:(fun { Entry.names; _ } ->
            not (List.is_empty names.left))
        in
        pp_print_newlines ppf 1;
        Ansi_style.pp_with_style style.section_heading ppf ~f:(fun ppf ->
          Format.pp_print_string ppf t.section_heading);
        pp_print_newlines ppf 1;
        let right_names_left_padding = max_left_length t in
        let doc_left_padding =
          max_name_length ~at_least_one_left_name ~right_names_left_padding t
        in
        List.iter t.entries ~f:(fun entry ->
          Entry.pp_padded
            style
            ppf
            ~at_least_one_left_name
            ~right_names_left_padding
            ~doc_left_padding
            entry))
    ;;
  end
end

module Positional_args = struct
  include Command_doc_spec.Positional_args

  let to_print_section { fixed; repeated } =
    let entries =
      List.map fixed ~f:(fun { Command_doc_spec.Positional_arg.value; doc } ->
        { Print.Entry.names = Print.Names.empty
        ; value = Some value
        ; doc
        ; repeated = false
        })
      |> List.append
           (Option.map repeated ~f:(fun { Command_doc_spec.Positional_arg.value; doc } ->
              { Print.Entry.names = Print.Names.empty
              ; value = Some value
              ; doc
              ; repeated = true
              })
            |> Option.to_list)
    in
    { Print.Section.section_heading = "Arguments:"; entries }
  ;;

  let pp style ppf t = Print.Section.pp style ppf (to_print_section t)
end

module Named_args = struct
  include Command_doc_spec.Named_args

  let to_print_section t =
    { Print.Section.section_heading = "Options:"
    ; entries =
        List.map
          t
          ~f:
            (fun
              { Command_doc_spec.Named_arg.names
              ; value
              ; repeated
              ; doc
              ; default_string = _
              }
            ->
            let names = Nonempty_list.to_list names in
            let short_names =
              List.filter names ~f:Name.is_short |> List.map ~f:Name.to_string_with_dashes
            in
            let long_names =
              List.filter names ~f:Name.is_long |> List.map ~f:Name.to_string_with_dashes
            in
            let names = { Print.Names.left = short_names; right = long_names } in
            { Print.Entry.names; value; doc; repeated })
    }
  ;;

  let pp style ppf t = Print.Section.pp style ppf (to_print_section t)
end

module Subcommands = struct
  include Command_doc_spec.Subcommands

  let to_print_section t =
    { Print.Section.section_heading = "Commands:"
    ; entries =
        List.map t ~f:(fun { Command_doc_spec.Subcommand.name; doc; aliases; _ } ->
          { Print.Entry.names =
              Print.Names.of_right
                (Name.to_string name :: List.map aliases ~f:Name.to_string)
          ; value = None
          ; doc
          ; repeated = false
          })
    }
  ;;

  let pp style ppf t = Print.Section.pp style ppf (to_print_section t)
end

let pp_command_base ppf (spec : Command_doc_spec.t) =
  Format.fprintf ppf "%s" spec.program_name;
  List.iter spec.subcommand ~f:(Format.fprintf ppf " %s")
;;

let pp_usage (style : Style.t) ppf (spec : Command_doc_spec.t) =
  Ansi_style.pp_with_style style.section_heading ppf ~f:(fun ppf ->
    Format.pp_print_string ppf "Usage: ");
  Ansi_style.pp_with_style style.usage ppf ~f:(fun ppf ->
    if not (List.is_empty spec.subcommands)
    then (
      pp_command_base ppf spec;
      Format.pp_print_string ppf " [COMMAND]";
      Format.pp_print_newline ppf ();
      Format.pp_print_string ppf "       ");
    pp_command_base ppf spec;
    Command_doc_spec.Args.pp_usage_args ~format_positional_args:Fun.id ppf spec.args);
  pp_print_newlines ppf 1
;;

let pp (style : Style.t) ppf (spec : Command_doc_spec.t) =
  Option.iter spec.doc ~f:(fun spec ->
    Ansi_style.pp_with_style style.program_doc ppf ~f:(fun ppf ->
      Format.pp_print_string ppf spec);
    pp_print_newlines ppf 2);
  pp_usage style ppf spec;
  Positional_args.pp style ppf spec.args.positional;
  Named_args.pp style ppf spec.args.named;
  Subcommands.pp style ppf spec.subcommands
;;
