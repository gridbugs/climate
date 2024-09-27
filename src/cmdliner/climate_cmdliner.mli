(** A wrapper of climate that copies a subset of cmdliner's api
    allowing climate to be used as a drop-in replacement for cmdliner,
    mostly for the purpose of testing climate in real-world programs such
    as dune. *)

module Manpage : sig
  type block =
    [ `S of string
    | `P of string
    | `Pre of string
    | `I of string * string
    | `Noblank
    | `Blocks of block list
    ]

  val s_name : string
  val s_synopsis : string
  val s_description : string
  val s_commands : string
  val s_command_aliases : string
  val s_arguments : string
  val s_options : string
  val s_common_options : string
  val s_exit_status : string
  val s_exit_status_intro : block
  val s_environment : string
  val s_environment_intro : block
  val s_files : string
  val s_bugs : string
  val s_examples : string
  val s_authors : string
  val s_see_also : string
  val s_none : string

  type title = string * int * string * string * string
  type t = title * block list

  type format =
    [ `Auto
    | `Pager
    | `Plain
    | `Groff
    ]

  val print : format -> Format.formatter -> t -> unit
end

module Term : sig
  type 'a t

  val const : 'a -> 'a t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

  type 'a ret =
    [ `Ok of 'a
    | `Error of bool * string
    | `Help of Manpage.format * string option
    ]

  val ret : 'a ret t -> 'a t
end

module Cmd : sig
  module Exit : sig
    type info

    val info : ?doc:string -> int -> info
  end

  module Env : sig
    type info

    val info : ?doc:string -> string -> info
  end

  type 'a t
  type info

  val info
    :  ?docs:string
    -> ?doc:string
    -> ?man:_
    -> ?envs:_
    -> ?version:_
    -> ?exits:Exit.info list
    -> string
    -> info

  val name : _ t -> string
  val v : info -> 'a Term.t -> 'a t
  val group : ?default:'a Term.t -> info -> 'a t list -> 'a t
  val print_completion_script_bash : string -> _ t
  val eval_value : ?catch:_ -> 'a t -> ('a, _) result
end

module Arg : sig
  type 'a t
  type 'a parser = string -> [ `Ok of 'a | `Error of string ]
  type 'a printer = Format.formatter -> 'a -> unit
  type 'a conv = 'a parser * 'a printer
  type info

  val ( & ) : ('a -> 'b) -> 'a -> 'b
  val value : 'a t -> 'a Term.t
  val required : 'a option t -> 'a Term.t

  val info
    :  ?docs:string
    -> ?docv:string
    -> ?doc:string
    -> ?env:Cmd.Env.info
    -> string list
    -> info

  val some : ?none:string -> 'a conv -> 'a option conv
  val enum : (string * 'a) list -> 'a conv
  val string : string conv
  val bool : bool conv
  val dir : string conv
  val file : string conv
  val int : int conv
  val float : float conv
  val opt : 'a conv -> 'a -> info -> 'a t
  val flag : info -> bool t
  val pos_all : 'a conv -> 'a list -> info -> 'a list t
  val pos_right : int -> 'a conv -> 'a list -> info -> 'a list t
  val pos : int -> 'a conv -> 'a -> info -> 'a t
  val opt_all : 'a conv -> 'a list -> info -> 'a list t
  val list : ?sep:char -> 'a conv -> 'a list conv

  val conv
    :  ?docv:string
    -> (string -> ('a, [ `Msg of string ]) result) * 'a printer
    -> 'a conv

  val conv' : (string -> ('a, string) result) * 'a printer -> 'a conv
  val conv_printer : 'a conv -> 'a printer
  val conv_parser : 'a conv -> 'a parser
  val last : 'a list t -> 'a Term.t
  val man_format : Manpage.format Term.t
  val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
  val doc_alts_enum : (string * 'a) list -> string
end
