open! Import

module Value = struct
  type t =
    { name : string
        (* The value name to be used in documentation, such as
           the "MESSAGE" in "--commit MESSAGE" *)
    ; required : bool
    }

  let pp ~format_name ppf t =
    if t.required
    then Format.fprintf ppf "<%s>" (format_name t.name)
    else Format.fprintf ppf "[%s]" (format_name t.name)
  ;;
end

module Positional_arg = struct
  type t =
    { value : Value.t
    ; doc : string option
    }
end

module Positional_args = struct
  type t =
    { fixed : Positional_arg.t list
    ; repeated : Positional_arg.t option
    }

  let pp_usage_args ~format_name ppf t =
    List.iter t.fixed ~f:(fun { Positional_arg.value; _ } ->
      Format.pp_print_string ppf " ";
      Value.pp ~format_name ppf value);
    Option.iter t.repeated ~f:(fun { Positional_arg.value; _ } ->
      Format.pp_print_string ppf " ";
      Value.pp ~format_name ppf value;
      Format.pp_print_string ppf "…")
  ;;
end

module Named_arg = struct
  type t =
    { names : Name.t Nonempty_list.t
    ; value : Value.t option
    ; repeated : bool
    ; default_string : string option
    ; doc : string option
    }
end

module Named_args = struct
  type t = Named_arg.t list
end

module Args = struct
  type t =
    { named : Named_args.t
    ; positional : Positional_args.t
    }

  let pp_usage_args ~format_positional_args ppf t =
    if not (List.is_empty t.named)
    then Format.fprintf ppf " [%s]…" (format_positional_args "OPTION");
    Positional_args.pp_usage_args ~format_name:format_positional_args ppf t.positional
  ;;
end

module Subcommand = struct
  type t =
    { name : Name.t
    ; doc : string option
    ; args : Args.t
    }
end

module Subcommands = struct
  type t = Subcommand.t list
end

type t =
  { program_name : string
  ; subcommand : string list
  ; doc : string option
  ; args : Args.t
  ; subcommands : Subcommands.t
  }
