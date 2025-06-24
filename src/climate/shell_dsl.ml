open! Import

type global_name = { suffix : string }

module Case_pattern = struct
  type t = string Nonempty_list.t

  let singleton = Nonempty_list.singleton
  let of_strings = Fun.id
  let union = Nonempty_list.concat
  let equal = Nonempty_list.equal ~eq:String.equal
end

module Local_variable = struct
  type t =
    { full_name : string
    ; short_name : string
    }

  let create ?short_name full_name =
    let short_name = Option.value short_name ~default:full_name in
    { full_name; short_name }
  ;;

  let to_string ~style { full_name; short_name } =
    match style with
    | `Full -> full_name
    | `Short -> short_name
  ;;

  let equal a b =
    String.equal a.full_name b.full_name && String.equal a.short_name b.short_name
  ;;
end

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
  | Local_variable of Local_variable.t
  | Argument of int
  | Literal_with_local_variable of (Local_variable.t * (string -> string))

and cond =
  | True
  | Call of function_call
  | Test_raw_cond of string
  | Test_raw_cond_of_string_with_global_name of string_with_global_name
  | Test_raw_cond_of_string_with_local_variable of (Local_variable.t * (string -> string))
  | Raw_cond_of_string_with_local_variable of (Local_variable.t * (string -> string))

and conditional_block =
  { cond : cond
  ; body : stmt list
  }

and if_ =
  { if_ : conditional_block
  ; elifs : conditional_block list
  ; else_ : stmt list option
  }

and case =
  { pattern : Case_pattern.t
  ; case_body : stmt list
  }

and case_stmt =
  { case_value : value
  ; cases : case list
  }

and stmt =
  | Raw of string
  | Raw_with_global_name of string_with_global_name
  | Cond of cond
  | If of if_
  | Case of case_stmt
  | While of conditional_block
  | Return of value
  | Comment of string
  | Noop
  | Declare_local_variables of (Local_variable.t * value option) list
  | Raw_with_local_variable of (Local_variable.t * (string -> string))
  | Raw_with_local_variable2 of
      (Local_variable.t * Local_variable.t * (string -> string -> string))
  | Raw_with_local_variable_and_global_name of
      (Local_variable.t * global_name * (string -> string -> string))
  | With_stdin_from_command of
      { stmt : stmt
      ; command_string : string
      }

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

  let local_variable local_variable = Local_variable local_variable
  let argument index = Argument index

  let literal_with_local_variable local_variable ~f =
    Literal_with_local_variable (local_variable, f)
  ;;

  let equal a b =
    match a, b with
    | Literal a, Literal b -> String.equal a b
    | Global a, Global b ->
      String.equal (Global_name.suffix a.name) (Global_name.suffix b.name)
    | Local_variable a, Local_variable b -> Local_variable.equal a b
    | Argument a, Argument b -> Int.equal a b
    | Literal_with_local_variable (var_a, f_a), Literal_with_local_variable (var_b, f_b)
      ->
      String.equal
        (f_a var_a.Local_variable.full_name)
        (f_b var_b.Local_variable.full_name)
    | _ -> false
  ;;
end

module Function_call = struct
  let equal a b =
    String.equal a.function_.name.suffix b.function_.name.suffix
    && List.equal a.args b.args ~eq:Value.equal
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

  let test_raw_of_string_with_local_variable ~f v =
    Test_raw_cond_of_string_with_local_variable (v, f)
  ;;

  let raw_of_string_with_local_variable ~f v =
    Raw_cond_of_string_with_local_variable (v, f)
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

  let equal a b =
    match a, b with
    | True, True -> true
    | Call a, Call b -> Function_call.equal a b
    | Test_raw_cond a, Test_raw_cond b -> String.equal a b
    | ( Test_raw_cond_of_string_with_global_name a
      , Test_raw_cond_of_string_with_global_name b ) ->
      String.equal
        (a.string_of_name a.global_name.suffix)
        (b.string_of_name b.global_name.suffix)
    | ( Test_raw_cond_of_string_with_local_variable (var_a, f_a)
      , Test_raw_cond_of_string_with_local_variable (var_b, f_b) ) ->
      String.equal
        (f_a var_a.Local_variable.full_name)
        (f_b var_b.Local_variable.full_name)
    | ( Raw_cond_of_string_with_local_variable (var_a, f_a)
      , Raw_cond_of_string_with_local_variable (var_b, f_b) ) ->
      String.equal
        (f_a var_a.Local_variable.full_name)
        (f_b var_b.Local_variable.full_name)
    | _ -> false
  ;;
end

module Stmt = struct
  type t = stmt

  let rec equal a b =
    match a, b with
    | Raw a, Raw b -> String.equal a b
    | Raw_with_global_name a, Raw_with_global_name b ->
      String.equal
        (a.string_of_name a.global_name.suffix)
        (b.string_of_name b.global_name.suffix)
    | Cond a, Cond b -> Cond.equal a b
    | If a, If b -> if_equal a b
    | ( Case { case_value = case_value_a; cases = cases_a }
      , Case { case_value = case_value_b; cases = cases_b } ) ->
      Value.equal case_value_a case_value_b && List.equal cases_a cases_b ~eq:case_equal
    | While a, While b -> conditional_block_equal a b
    | Return a, Return b -> Value.equal a b
    | Comment _, Comment _ -> true
    | Noop, Noop -> true
    | Declare_local_variables a, Declare_local_variables b ->
      List.equal
        a
        b
        ~eq:(fun (local_variable_a, value_opt_a) (local_variable_b, value_opt_b) ->
          Local_variable.equal local_variable_a local_variable_b
          && Option.equal value_opt_a value_opt_b ~eq:Value.equal)
    | Raw_with_local_variable _, Raw_with_local_variable _ -> (* TODO *) false
    | Raw_with_local_variable2 _, Raw_with_local_variable2 _ -> (* TODO *) false
    | Raw_with_local_variable_and_global_name _, Raw_with_local_variable_and_global_name _
      -> (* TODO *) false
    | _ -> false

  and conditional_block_equal a b =
    Cond.equal a.cond b.cond && List.equal a.body b.body ~eq:equal

  and if_equal a b =
    conditional_block_equal a.if_ b.if_
    && List.equal a.elifs b.elifs ~eq:conditional_block_equal
    && Option.equal a.else_ b.else_ ~eq:(List.equal ~eq:equal)

  and case_equal a b =
    Case_pattern.equal a.pattern b.pattern && List.equal a.case_body b.case_body ~eq:equal
  ;;

  let raw s = Raw s

  let raw_with_global_name ~f global_name =
    Raw_with_global_name { string_of_name = f; global_name }
  ;;

  let call function_ args = Cond (Cond.call function_ args)

  let test_raw_cond_of_string_with_global_name ~f global_named_value =
    Cond (Cond.test_raw_of_string_with_global_name ~f global_named_value)
  ;;

  let raw_cond_of_string_with_local_variable ~f global_named_value =
    Cond (Cond.raw_of_string_with_local_variable ~f global_named_value)
  ;;

  let if_ ?elifs ?else_ cond body =
    let elifs =
      Option.map elifs ~f:(List.map ~f:(fun (cond, body) -> { cond; body }))
      |> Option.value ~default:[]
    in
    If { if_ = { cond; body }; elifs; else_ }
  ;;

  let case case_value patterns =
    let cases =
      List.map patterns ~f:(fun (pattern, case_body) -> { pattern; case_body })
    in
    Case { case_value; cases }
  ;;

  let while_ cond body = While { cond; body }
  let return s = Return s
  let local_decl local_variable = local_variable, None
  let local_init local_variable value = local_variable, Some value
  let declare_local_variables vars = Declare_local_variables vars
  let raw_with_local_variable var ~f = Raw_with_local_variable (var, f)
  let raw_with_local_variable2 var0 var1 ~f = Raw_with_local_variable2 (var0, var1, f)

  let raw_with_local_variable_and_global_name local_variable global_name ~f =
    Raw_with_local_variable_and_global_name (local_variable, global_name, f)
  ;;

  let with_stdin_from_command stmt ~command_string =
    With_stdin_from_command { stmt; command_string }
  ;;

  let comment s = Comment s
  let noop = Noop

  let is_comment = function
    | Comment _ -> true
    | _ -> false
  ;;

  let map_global_name_suffix_single_stmt t ~f =
    match t with
    | Raw _
    | Noop
    | Comment _
    | Raw_with_local_variable _
    | Raw_with_local_variable2 _
    | With_stdin_from_command _ -> t
    | Raw_with_global_name { string_of_name; global_name } ->
      Raw_with_global_name
        { string_of_name; global_name = Global_name.with_suffix global_name ~f }
    | Raw_with_local_variable_and_global_name (local_variable, global_name, g) ->
      Raw_with_local_variable_and_global_name
        (local_variable, Global_name.with_suffix global_name ~f, g)
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
    | Case { case_value; cases } ->
      Case { case_value = Value.map_global_name_suffix case_value ~f; cases }
    | Return value -> Return (Value.map_global_name_suffix value ~f)
    | Declare_local_variables decls ->
      Declare_local_variables
        (List.map decls ~f:(function
          | variable, Some value -> variable, Some (Value.map_global_name_suffix value ~f)
          | other -> other))
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
      | Raw _
      | Raw_with_global_name _
      | Raw_with_local_variable_and_global_name _
      | Cond _
      | Return _
      | Comment _
      | Noop
      | Declare_local_variables _
      | Raw_with_local_variable _
      | Raw_with_local_variable2 _ -> t, acc
      | With_stdin_from_command { stmt; command_string } ->
        let stmt, acc = single stmt acc in
        With_stdin_from_command { stmt; command_string }, acc
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
      | Case { case_value; cases } ->
        let rev_cases, acc =
          List.fold_left
            cases
            ~init:([], acc)
            ~f:(fun (rev_cases, acc) { pattern; case_body } ->
              let case_body, acc = loop case_body acc in
              { pattern; case_body } :: rev_cases, acc)
        in
        let cases = List.rev rev_cases in
        Case { case_value; cases }, acc
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

  let strip_comments_and_noops =
    transform_blocks_top_down
      ~f:
        (List.filter ~f:(function
          | Noop | Comment _ -> false
          | _ -> true))
  ;;

  let equal_ignoring_comments_and_noops a b =
    List.equal (strip_comments_and_noops a) (strip_comments_and_noops b) ~eq:equal
  ;;

  let optimize_case_stmt { case_value; cases } =
    (* replace contiguous sequences of cases with identical bodies with single
       cases combining the patterns of each identical case. *)
    let rec loop = function
      | [] -> []
      | [ x ] -> [ x ]
      | a :: b :: rest ->
        if equal_ignoring_comments_and_noops a.case_body b.case_body
        then (
          (* merge a and b *)
          let merged_pattern = Case_pattern.union [ a.pattern; b.pattern ] in
          let case_body =
            match a.case_body with
            | Comment "Automatically merged case bodies" :: _ -> a.case_body
            | _ -> Comment "Automatically merged case bodies" :: a.case_body
          in
          loop ({ pattern = merged_pattern; case_body } :: rest))
        else a :: loop (b :: rest)
    in
    { case_value; cases = loop cases }
  ;;

  let optimize_case_stmts =
    transform_blocks_top_down
      ~f:
        (List.map ~f:(function
          | Case case_stmt -> Case (optimize_case_stmt case_stmt)
          | other -> other))
  ;;
end

type ctx =
  { global_symbol_prefix : string
  ; local_variable_style : [ `Full | `Short ]
  }

module Bash = struct
  let rec value_to_string ~ctx = function
    | Literal s -> sprintf "\"%s\"" s
    | Global global_named_value ->
      sprintf
        "\"$%s\""
        (Global_named_value.global_name_with_prefix
           ~global_symbol_prefix:ctx.global_symbol_prefix
           global_named_value)
    | Local_variable local_variable ->
      sprintf
        "\"$%s\""
        (Local_variable.to_string ~style:ctx.local_variable_style local_variable)
    | Argument index -> sprintf "\"$%d\"" index
    | Literal_with_local_variable (v, f) ->
      let variable_string =
        sprintf "%s" (Local_variable.to_string ~style:ctx.local_variable_style v)
      in
      sprintf "\"%s\"" (f variable_string)

  and function_call_to_string ~ctx { function_; args } =
    let function_name =
      Global_named_value.global_name_with_prefix
        ~global_symbol_prefix:ctx.global_symbol_prefix
        function_
    in
    let args_strings = List.map args ~f:(value_to_string ~ctx) in
    String.concat ~sep:" " (function_name :: args_strings)
  ;;

  let cond_to_string ~ctx = function
    | True -> "true"
    | Call function_call -> function_call_to_string ~ctx function_call
    | Test_raw_cond s -> sprintf "[ %s ]" s
    | Test_raw_cond_of_string_with_global_name string_with_global_name ->
      let s =
        Global_named_value.string_with_global_name_to_string
          ~global_symbol_prefix:ctx.global_symbol_prefix
          string_with_global_name
      in
      sprintf "[ %s ]" s
    | Test_raw_cond_of_string_with_local_variable (v, f) ->
      let s = Local_variable.to_string ~style:ctx.local_variable_style v in
      sprintf "[ %s ]" (f s)
    | Raw_cond_of_string_with_local_variable (v, f) ->
      let s = Local_variable.to_string ~style:ctx.local_variable_style v in
      f s
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

  let stmt_to_bash_lines_with_indent ~ctx ~indent =
    let rec loop indent = function
      | Raw text -> [ { indent; text } ]
      | Raw_with_global_name string_with_global_name ->
        let text =
          Global_named_value.string_with_global_name_to_string
            ~global_symbol_prefix:ctx.global_symbol_prefix
            string_with_global_name
        in
        [ { text; indent } ]
      | Raw_with_local_variable_and_global_name (local_variable, global_name, f) ->
        let text =
          let global_name_string =
            Global_name.with_prefix
              ~global_symbol_prefix:ctx.global_symbol_prefix
              global_name
          in
          let local_variable_string =
            Local_variable.to_string ~style:ctx.local_variable_style local_variable
          in
          f local_variable_string global_name_string
        in
        [ { indent; text } ]
      | Cond c ->
        let text = cond_to_string ~ctx c in
        [ { text; indent } ]
      | If { if_; elifs; else_ } ->
        ({ indent; text = sprintf "if %s; then" (cond_to_string ~ctx if_.cond) }
         :: List.concat_map if_.body ~f:(loop (indent + 1)))
        @ List.concat_map elifs ~f:(fun { cond; body } ->
          { indent; text = sprintf "elif %s; then" (cond_to_string ~ctx cond) }
          :: List.concat_map body ~f:(loop (indent + 1)))
        @ (match else_ with
           | None -> []
           | Some stmts ->
             { indent; text = "else" } :: List.concat_map stmts ~f:(loop (indent + 1)))
        @ [ { indent; text = "fi" } ]
      | Case { case_value; cases } ->
        ({ indent; text = sprintf "case %s in" (value_to_string ~ctx case_value) }
         :: List.concat_map cases ~f:(fun { pattern; case_body } ->
           let pattern_string =
             String.concat ~sep:" | " (Nonempty_list.to_list pattern)
           in
           ({ indent = indent + 1; text = sprintf "%s)" pattern_string }
            :: List.concat_map case_body ~f:(loop (indent + 2)))
           @ [ { indent = indent + 2; text = ";;" } ]))
        @ [ { indent; text = "esac" } ]
      | While { cond; body } ->
        ({ indent; text = sprintf "while %s; do" (cond_to_string ~ctx cond) }
         :: List.concat_map body ~f:(loop (indent + 1)))
        @ [ { indent; text = "done" } ]
      | Return value ->
        [ { indent; text = sprintf "return %s" (value_to_string ~ctx value) } ]
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
      | Declare_local_variables vars ->
        [ { indent
          ; text =
              String.concat
                ~sep:" "
                ("local"
                 :: List.map vars ~f:(function
                   | variable, None ->
                     Local_variable.to_string ~style:ctx.local_variable_style variable
                   | variable, Some value ->
                     sprintf
                       "%s=%s"
                       (Local_variable.to_string ~style:ctx.local_variable_style variable)
                       (value_to_string ~ctx value)))
          }
        ]
      | Raw_with_local_variable (var, f) ->
        let var_string = Local_variable.to_string ~style:ctx.local_variable_style var in
        [ { indent; text = f var_string } ]
      | Raw_with_local_variable2 (var0, var1, f) ->
        let var0_string = Local_variable.to_string ~style:ctx.local_variable_style var0 in
        let var1_string = Local_variable.to_string ~style:ctx.local_variable_style var1 in
        [ { indent; text = f var0_string var1_string } ]
      | With_stdin_from_command { stmt; command_string } ->
        let rec ammend_last = function
          | [] -> []
          | [ { indent; text } ] ->
            [ { indent; text = sprintf "%s < <(%s)" text command_string } ]
          | x :: xs -> x :: ammend_last xs
        in
        ammend_last (loop indent stmt)
    in
    loop indent
  ;;

  let remove_comments =
    List.filter ~f:(function
      | Comment _ -> false
      | _ -> true)
  ;;

  let global_named_value_to_lines
    ~global_symbol_prefix
    ~local_variable_style
    { name; value }
    =
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
          ~f:
            (stmt_to_bash_lines_with_indent
               ~ctx:{ global_symbol_prefix; local_variable_style }
               ~indent:1)
      in
      ({ indent = 0; text = sprintf "%s() {" name } :: body)
      @ [ { indent = 0; text = "}" } ]
  ;;

  let lines_to_string ~indent_size lines =
    List.map lines ~f:(fun { indent; text } ->
      let indent_string = String.make (indent * indent_size) ' ' in
      String.cat indent_string text)
    |> String.concat ~sep:"\n"
  ;;

  let global_named_value_to_string
    ~global_symbol_prefix
    ~local_variable_style
    ~indent_size
    global_named_value
    =
    global_named_value_to_lines
      ~global_symbol_prefix
      ~local_variable_style
      global_named_value
    |> lines_to_string ~indent_size
  ;;

  let stmt_to_string ~global_symbol_prefix ~local_variable_style ~indent_size stmt =
    stmt_to_bash_lines_with_indent
      ~ctx:{ global_symbol_prefix; local_variable_style }
      ~indent:0
      stmt
    |> lines_to_string ~indent_size
  ;;
end
