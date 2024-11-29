open Import

module Color = struct
  type t =
    [ `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    ]
end

type t =
  { bold : bool
  ; underline : bool
  ; color : Color.t option
  }

let default = { bold = false; underline = false; color = None }
let reset = "\x1b[0m"

let escape { bold; underline; color } =
  let effects =
    List.append (if bold then [ ";1" ] else []) (if underline then [ ";4" ] else [])
  in
  let color_code =
    match (color : Color.t option) with
    | None -> 0
    | Some `Red -> 31
    | Some `Green -> 32
    | Some `Yellow -> 33
    | Some `Blue -> 34
    | Some `Magenta -> 35
    | Some `Cyan -> 36
  in
  Printf.sprintf "\x1b[%d%sm" color_code (String.concat ~sep:"" effects)
;;

let pp_with_style t ppf ~f =
  Format.pp_print_string ppf (escape t);
  f ppf;
  Format.pp_print_string ppf reset
;;
