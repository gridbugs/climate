module Color : sig
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

val default : t
val pp_with_style : t -> Format.formatter -> f:(Format.formatter -> unit) -> unit
