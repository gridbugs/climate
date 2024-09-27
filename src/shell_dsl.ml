open! Import

type global_name = { suffix : string }

type global_value =
  | Global_variable of { initial_value : string }
  | Function of { body : stmt list }

and global_named_value =
  { name : global_name
  ; value : global_value
  }

and function_call =
  { function_ : global_named_value
  ; args : value list
  }

and string_with_global_name =
  { string_of_name : string -> string
  ; global_name : global_name
  }

and value =
  | Literal of string
  | Global of global_named_value

and cond =
  | True
  | Call of function_call
  | Test_raw_cond of string
  | Test_raw_cond_of_string_with_global_name of string_with_global_name

and conditional_block =
  { cond : cond
  ; body : stmt list
  }

and if_ =
  { if_ : conditional_block
  ; elifs : conditional_block list
  ; else_ : stmt list option
  }

and case_pattern =
  { pattern : string Nonempty_list.t
  ; case_body : stmt list
  }

and stmt =
  | Raw of string
  | Raw_with_global_name of string_with_global_name
  | Cond of cond
  | If of if_
  | Case of
      { value : value
      ; patterns : case_pattern list
      }
  | While of conditional_block
  | Return of value
  | Comment of string
  | Noop

module Global_name = struct
  type t = global_name

  let make suffix = { suffix }

  let with_prefix ~global_symbol_prefix { suffix } =
    String.cat global_symbol_prefix suffix
  ;;

  let suffix t = t.suffix
  let with_suffix t ~f = { suffix = f t.suffix }
end

module Global_named_value = struct
  type t = global_named_value

  let name t = t.name
  let with_suffix t ~f = { t with name = Global_name.with_suffix t.name ~f }

  let global_name_with_prefix ~global_symbol_prefix t =
    Global_name.with_prefix t.name ~global_symbol_prefix
  ;;

  let string_with_global_name_to_string
    ~global_symbol_prefix
    { string_of_name; global_name }
    =
    string_of_name (Global_name.with_prefix ~global_symbol_prefix global_name)
  ;;

  let global_variable ~name ~initial_value =
    { name = Global_name.make name; value = Global_variable { initial_value } }
  ;;

  let function_ name body = { name = Global_name.make name; value = Function { body } }

  let with_function_stmts ({ value; _ } as t) ~f =
    match value with
    | Global_variable _ -> t
    | Function { body } -> { t with value = Function { body = f body } }
  ;;
end

module Value = struct
  type t = value

  let literal s = Literal s
  let global global_named_value = Global global_named_value

  let map_global_name_suffix t ~f =
    match t with
    | Global g -> Global (Global_named_value.with_suffix g ~f)
    | _ -> t
  ;;
end

module Cond = struct
  type t = cond

  let true_ = True
  let call function_ args = Call { function_; args }
  let test_raw s = Test_raw_cond s

  let test_raw_of_string_with_global_name ~f global_name =
    Test_raw_cond_of_string_with_global_name { string_of_name = f; global_name }
  ;;

  let map_global_name_suffix t ~f =
    match t with
    | Call { function_; args } ->
      let function_ = Global_named_value.with_suffix function_ ~f in
      let args = List.map args ~f:(Value.map_global_name_suffix ~f) in
      Call { function_; args }
    | Test_raw_cond_of_string_with_global_name { string_of_name; global_name } ->
      Test_raw_cond_of_string_with_global_name
        { string_of_name; global_name = Global_name.with_suffix global_name ~f }
    | _ -> t
  ;;
end

module Stmt = struct
  type t = stmt

  let raw s = Raw s

  let raw_with_global_name ~f global_name =
    Raw_with_global_name { string_of_name = f; global_name }
  ;;

  let call function_ args = Cond (Cond.call function_ args)

  let test_raw_cond_of_string_with_global_name ~f global_named_value =
    Cond (Cond.test_raw_of_string_with_global_name ~f global_named_value)
  ;;

  let if_ ?elifs ?else_ cond body =
    let elifs =
      Option.map elifs ~f:(List.map ~f:(fun (cond, body) -> { cond; body }))
      |> Option.value ~default:[]
    in
    If { if_ = { cond; body }; elifs; else_ }
  ;;

  type patterns = string Nonempty_list.t

  let pattern = Nonempty_list.singleton
  let patterns = Fun.id

  let case value patterns =
    let patterns =
      List.map patterns ~f:(fun (pattern, case_body) -> { pattern; case_body })
    in
    Case { value; patterns }
  ;;

  let while_ cond body = While { cond; body }
  let return s = Return s
  let comment s = Comment s
  let noop = Noop

  let is_comment = function
    | Comment _ -> true
    | _ -> false
  ;;

  let map_global_name_suffix_single_stmt t ~f =
    match t with
    | Raw_with_global_name { string_of_name; global_name } ->
      Raw_with_global_name
        { string_of_name; global_name = Global_name.with_suffix global_name ~f }
    | Cond cond -> Cond (Cond.map_global_name_suffix cond ~f)
    | If { if_ = { cond; body }; elifs; else_ } ->
      let cond = Cond.map_global_name_suffix cond ~f in
      let elifs =
        List.map elifs ~f:(fun { cond; body } ->
          let cond = Cond.map_global_name_suffix cond ~f in
          { cond; body })
      in
      If { if_ = { cond; body }; elifs; else_ }
    | While { cond; body } ->
      let cond = Cond.map_global_name_suffix cond ~f in
      While { cond; body }
    | Case { value; patterns } ->
      Case { value = Value.map_global_name_suffix value ~f; patterns }
    | Return value -> Return (Value.map_global_name_suffix value ~f)
    | _ -> t
  ;;

  let fold_blocks_top_down ts ~init ~f =
    let rec loop ts acc =
      let ts, acc = f ts acc in
      let rev_ts, acc =
        List.fold_left ts ~init:([], acc) ~f:(fun (rev_ts, acc) t ->
          let t, acc = single t acc in
          t :: rev_ts, acc)
      in
      List.rev rev_ts, acc
    and single t acc =
      match t with
      | Raw _ | Raw_with_global_name _ | Cond _ | Return _ | Comment _ | Noop -> t, acc
      | If { if_ = { cond; body }; elifs; else_ } ->
        let body, acc = loop body acc in
        let rev_elifs, acc =
          List.fold_left elifs ~init:([], acc) ~f:(fun (rev_elifs, acc) { cond; body } ->
            let body, acc = loop body acc in
            { cond; body } :: rev_elifs, acc)
        in
        let elifs = List.rev rev_elifs in
        let else_, acc =
          match else_ with
          | None -> None, acc
          | Some else_ ->
            let else_, acc = loop else_ acc in
            Some else_, acc
        in
        If { if_ = { cond; body }; elifs; else_ }, acc
      | Case { value; patterns } ->
        let rev_patterns, acc =
          List.fold_left
            patterns
            ~init:([], acc)
            ~f:(fun (rev_patterns, acc) { pattern; case_body } ->
              let case_body, acc = loop case_body acc in
              { pattern; case_body } :: rev_patterns, acc)
        in
        let patterns = List.rev rev_patterns in
        Case { value; patterns }, acc
      | While { cond; body } ->
        let body, acc = loop body acc in
        While { cond; body }, acc
    in
    loop ts init
  ;;

  let transform_blocks_top_down ts ~f =
    fst @@ fold_blocks_top_down ts ~init:() ~f:(fun ts () -> f ts, ())
  ;;

  let map_global_name_suffix ts ~f =
    transform_blocks_top_down ts ~f:(List.map ~f:(map_global_name_suffix_single_stmt ~f))
  ;;
end

module Bash = struct
  let rec value_to_string ~global_symbol_prefix = function
    | Literal s -> sprintf "\"%s\"" s
    | Global global_named_value ->
      sprintf
        "\"$%s\""
        (Global_named_value.global_name_with_prefix
           ~global_symbol_prefix
           global_named_value)

  and function_call_to_string ~global_symbol_prefix { function_; args } =
    let function_name =
      Global_named_value.global_name_with_prefix ~global_symbol_prefix function_
    in
    let args_strings = List.map args ~f:(value_to_string ~global_symbol_prefix) in
    String.concat ~sep:" " (function_name :: args_strings)
  ;;

  let cond_to_string ~global_symbol_prefix = function
    | True -> "true"
    | Call function_call -> function_call_to_string ~global_symbol_prefix function_call
    | Test_raw_cond s -> sprintf "[ %s ]" s
    | Test_raw_cond_of_string_with_global_name string_with_global_name ->
      let s =
        Global_named_value.string_with_global_name_to_string
          ~global_symbol_prefix
          string_with_global_name
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
        else (word :: last_line, new_last_line_length) :: xs)
    |> List.rev
    |> List.map ~f:(fun (line, _length) ->
      List.rev line |> String.concat ~sep:(String.make 1 sep))
  ;;

  let rec stmt_to_lines_with_indent ~global_symbol_prefix ~indent = function
    | Raw text -> [ { indent; text } ]
    | Raw_with_global_name string_with_global_name ->
      let text =
        Global_named_value.string_with_global_name_to_string
          ~global_symbol_prefix
          string_with_global_name
      in
      [ { text; indent } ]
    | Cond c ->
      let text = cond_to_string ~global_symbol_prefix c in
      [ { text; indent } ]
    | If { if_; elifs; else_ } ->
      ({ indent
       ; text = sprintf "if %s; then" (cond_to_string ~global_symbol_prefix if_.cond)
       }
       :: List.concat_map
            if_.body
            ~f:(stmt_to_lines_with_indent ~global_symbol_prefix ~indent:(indent + 1)))
      @ List.concat_map elifs ~f:(fun { cond; body } ->
        { indent
        ; text = sprintf "elif %s; then" (cond_to_string ~global_symbol_prefix cond)
        }
        :: List.concat_map
             body
             ~f:(stmt_to_lines_with_indent ~global_symbol_prefix ~indent:(indent + 1)))
      @ (match else_ with
         | None -> []
         | Some stmts ->
           { indent; text = "else" }
           :: List.concat_map
                stmts
                ~f:(stmt_to_lines_with_indent ~global_symbol_prefix ~indent:(indent + 1)))
      @ [ { indent; text = "fi" } ]
    | Case { value; patterns } ->
      ({ indent
       ; text = sprintf "case %s in" (value_to_string ~global_symbol_prefix value)
       }
       :: List.concat_map patterns ~f:(fun { pattern; case_body } ->
         let pattern_string = String.concat ~sep:" | " (Nonempty_list.to_list pattern) in
         ({ indent = indent + 1; text = sprintf "%s)" pattern_string }
          :: List.concat_map
               case_body
               ~f:(stmt_to_lines_with_indent ~global_symbol_prefix ~indent:(indent + 2)))
         @ [ { indent = indent + 2; text = ";;" } ]))
      @ [ { indent; text = "esac" } ]
    | While { cond; body } ->
      ({ indent
       ; text = sprintf "while %s; do" (cond_to_string ~global_symbol_prefix cond)
       }
       :: List.concat_map
            body
            ~f:(stmt_to_lines_with_indent ~global_symbol_prefix ~indent:(indent + 1)))
      @ [ { indent; text = "done" } ]
    | Return value ->
      [ { indent
        ; text = sprintf "return %s" (value_to_string ~global_symbol_prefix value)
        }
      ]
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
    | Noop -> [ { indent; text = ":" } ]
  ;;

  let remove_comments =
    List.filter ~f:(function
      | Comment _ -> false
      | _ -> true)
  ;;

  let global_named_value_to_lines ~global_symbol_prefix { name; value } =
    match value with
    | Global_variable { initial_value } ->
      let name = Global_name.with_prefix name ~global_symbol_prefix in
      [ { indent = 0; text = sprintf "%s=%s" name initial_value } ]
    | Function { body } ->
      let name = Global_name.with_prefix name ~global_symbol_prefix in
      let body =
        if List.is_empty (remove_comments body)
        then
          (* Add a noop because a function with an empty body is not
             allowed. *)
          body @ [ Noop ]
        else body
      in
      let body =
        List.concat_map
          body
          ~f:(stmt_to_lines_with_indent ~global_symbol_prefix ~indent:1)
      in
      ({ indent = 0; text = sprintf "%s() {" name } :: body)
      @ [ { indent = 0; text = "}" } ]
  ;;

  let lines_to_string lines =
    let indent_size = 4 in
    List.map lines ~f:(fun { indent; text } ->
      let indent_string = String.make (indent * indent_size) ' ' in
      String.cat indent_string text)
    |> String.concat ~sep:"\n"
  ;;

  let global_named_value_to_string ~global_symbol_prefix global_named_value =
    global_named_value_to_lines ~global_symbol_prefix global_named_value
    |> lines_to_string
  ;;

  let stmt_to_string ~global_symbol_prefix stmt =
    stmt_to_lines_with_indent ~indent:0 ~global_symbol_prefix stmt |> lines_to_string
  ;;
end
