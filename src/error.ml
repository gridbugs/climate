open! Import

let sprintf = Printf.sprintf

module Spec = struct
  type t =
    [ `Empty_name
    | `No_names
    | `Duplicate_name of Name.t
    | `Negative_position of int
    | `Duplicate_name_across_terms of Name.t * Name.t list * Name.t list
    | `Gap_in_positional_indices of [ `Gap of int ] * [ `Max of int ] ]

  let exit_code = 124

  let to_string = function
    | `Empty_name -> "The empty string cannot be used as a name."
    | `No_names -> "At least one name must be provided."
    | `Duplicate_name name ->
        sprintf "The name %S appeared twice which is not allowed."
          (Name.to_string name)
    | `Negative_position position ->
        sprintf "The position %d is negative which is not allowed." position
    | `Duplicate_name_across_terms (name, name_set1, name_set2) ->
        let name_set_to_string name_set =
          String.concat ~sep:", "
            (List.map name_set ~f:(fun name ->
                 sprintf "%S" (Name.to_string name)))
        in
        sprintf
          "The name %S was duplicated across multiple terms appearing as part \
           of the sets [%s] and [%s]"
          (Name.to_string name)
          (name_set_to_string name_set1)
          (name_set_to_string name_set2)
    | `Gap_in_positional_indices (`Gap gap, `Max max) ->
        sprintf
          "Gap in positional indices that will be parsed. The maximum \
           positional index is %d but there is no parser for the positional \
           argument at index %d."
          max gap

  let result_get = function
    | Ok x -> x
    | Error t ->
        Printf.eprintf "Invalid command spec: %s" (to_string t);
        exit exit_code
end
