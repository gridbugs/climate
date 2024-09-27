open! Import

(** A DSL for writing shell scripts intended for use in completion
    scripts. This provides some benefits over generating shell scripts
    directly with strings:

    - Formatting such as indentation and comment wrapping can be
      applied automatically and in a centralized place.

    - One could conceivably emit a completion script in other
      languages besides bash one day, though this will likely require
      some tweaking to make work. *)

(** Equal to [Stmt.t]. Exposed to break a dependency cycle with
    [Global_named_value.function_]. *)
type stmt

module Global_name : sig
  (** A global name which will have a unique prefix in the generated
      bash script so as to not conflict with other global names *)
  type t

  val make : string -> t
  val suffix : t -> string
end

module Global_named_value : sig
  type t

  val name : t -> Global_name.t
  val global_variable : name:string -> initial_value:string -> t
  val function_ : string -> stmt list -> t

  (** Transform the statements within t if t is a function *)
  val with_function_stmts : t -> f:(stmt list -> stmt list) -> t

  (** Transform the suffix of the name of the value *)
  val with_suffix : t -> f:(string -> string) -> t
end

module Value : sig
  type t

  val literal : string -> t
  val global : Global_named_value.t -> t
end

module Cond : sig
  type t

  val true_ : t
  val call : Global_named_value.t -> Value.t list -> t
  val test_raw : string -> t
  val test_raw_of_string_with_global_name : f:(string -> string) -> Global_name.t -> t
end

module Stmt : sig
  type t = stmt

  val raw : string -> t
  val raw_with_global_name : f:(string -> string) -> Global_name.t -> t
  val call : Global_named_value.t -> Value.t list -> t

  val test_raw_cond_of_string_with_global_name
    :  f:(string -> string)
    -> Global_name.t
    -> t

  val if_ : ?elifs:(Cond.t * t list) list -> ?else_:t list -> Cond.t -> t list -> t

  type patterns

  val pattern : string -> patterns
  val patterns : string Nonempty_list.t -> patterns
  val case : Value.t -> (patterns * t list) list -> t
  val while_ : Cond.t -> t list -> t
  val return : Value.t -> t
  val comment : string -> t
  val is_comment : t -> bool
  val noop : t

  (** Call a given function on all blocks appearing within the statement. *)
  val transform_blocks_top_down : t list -> f:(t list -> t list) -> t list

  (** Rename the suffix of all global names referenced within a block of statements *)
  val map_global_name_suffix : t list -> f:(string -> string) -> t list
end

module Bash : sig
  val global_named_value_to_string
    :  global_symbol_prefix:string
    -> Global_named_value.t
    -> string

  val stmt_to_string : global_symbol_prefix:string -> Stmt.t -> string
end
