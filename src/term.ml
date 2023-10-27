open! Import

type locator = Info.Term.Locator.t
type info = Info.Term.t

let named strings =
  Info.Term.Locator.named_of_strings strings |> Error.Spec.result_get

let positional int =
  Info.Term.Locator.at_position_of_int int |> Error.Spec.result_get

let all_positional = Info.Term.Locator.All_positional
let info locator = Info.Term.create locator
let ( & ) f a = f a

type 'a parser = Arg_table.t -> 'a
type 'a t = { infos : Info.Term.t list; parser : 'a parser }

let sealed_infos { infos; _ } = Info.Term.Sealed.create infos
let const value = { infos = []; parser = Fun.const value }
let map t ~f = { t with parser = (fun arg_table -> f (t.parser arg_table)) }

let both x y =
  {
    infos = x.infos @ y.infos;
    parser =
      (fun arg_table ->
        let x_value = x.parser arg_table in
        let y_value = y.parser arg_table in
        (x_value, y_value));
  }

let ( >>| ) t f = map t ~f
let ( let+ ) = ( >>| )
let ( and+ ) = both
let string info = { infos = [ info ]; parser = (fun arg_table -> "todo") }
