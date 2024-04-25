open! Import

let sprintf = Printf.sprintf

type global_name = unique_prefix:string -> string

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
  | Literal_with_global_name of string_with_global_name
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
  { pattern : string
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

  let with_prefix name ~unique_prefix = String.cat unique_prefix name
end

module Global_named_value = struct
  type t = global_named_value

  let name t = t.name
  let global_name_with_prefix ~unique_prefix t = t.name ~unique_prefix

  let string_with_global_name_to_string ~unique_prefix { string_of_name; global_name } =
    string_of_name (global_name ~unique_prefix)
  ;;

  let global_variable ~name ~initial_value =
    { name = Global_name.with_prefix name; value = Global_variable { initial_value } }
  ;;

  let function_ name body =
    { name = Global_name.with_prefix name; value = Function { body } }
  ;;
end

module Value = struct
  type t = value

  let literal s = Literal s

  let literal_with_global_name ~f global_name =
    Literal_with_global_name { string_of_name = f; global_name }
  ;;

  let global global_named_value = Global global_named_value
end

module Cond = struct
  type t = cond

  let true_ = True
  let call function_ args = Call { function_; args }
  let test_raw s = Test_raw_cond s

  let test_raw_of_string_with_global_name ~f global_name =
    Test_raw_cond_of_string_with_global_name { string_of_name = f; global_name }
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
end

module Bash = struct
  let rec value_to_string ~unique_prefix = function
    | Literal s -> sprintf "\"%s\"" s
    | Literal_with_global_name string_with_global_name ->
      let s =
        Global_named_value.string_with_global_name_to_string
          ~unique_prefix
          string_with_global_name
      in
      sprintf "\"%s\"" s
    | Global global_named_value ->
      sprintf
        "\"$%s\""
        (Global_named_value.global_name_with_prefix ~unique_prefix global_named_value)

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
    | Test_raw_cond_of_string_with_global_name string_with_global_name ->
      let s =
        Global_named_value.string_with_global_name_to_string
          ~unique_prefix
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

  let rec stmt_to_lines_with_indent ~unique_prefix ~indent = function
    | Raw text -> [ { indent; text } ]
    | Raw_with_global_name string_with_global_name ->
      let text =
        Global_named_value.string_with_global_name_to_string
          ~unique_prefix
          string_with_global_name
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
    | Noop -> [ { indent; text = ":" } ]
  ;;

  let remove_comments =
    List.filter ~f:(function
      | Comment _ -> false
      | _ -> true)
  ;;

  let global_named_value_to_lines ~unique_prefix { name; value } =
    match value with
    | Global_variable { initial_value } ->
      let name = name ~unique_prefix in
      [ { indent = 0; text = sprintf "%s=%s" name initial_value } ]
    | Function { body } ->
      let name = name ~unique_prefix in
      let body =
        if List.is_empty (remove_comments body)
        then
          (* Add a noop because a function with an empty body is not
             allowed. *)
          body @ [ Noop ]
        else body
      in
      let body =
        List.concat_map body ~f:(stmt_to_lines_with_indent ~unique_prefix ~indent:1)
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

  let global_named_value_to_string ~unique_prefix global_named_value =
    global_named_value_to_lines ~unique_prefix global_named_value |> lines_to_string
  ;;

  let stmt_to_string ~unique_prefix stmt =
    stmt_to_lines_with_indent ~indent:0 ~unique_prefix stmt |> lines_to_string
  ;;
end
