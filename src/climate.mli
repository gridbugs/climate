module Term : sig
  type 'a parse = string -> ('a, [ `Msg of string ]) result
  type 'a print = Format.formatter -> 'a -> unit
  type 'a conv = { parse : 'a parse; print : 'a print }

  val string : string conv
  val int : int conv
  val float : float conv
  val bool : bool conv
  val enum : (string * 'a) list -> eq:('a -> 'a -> bool) -> 'a conv

  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t

  module O : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  type 'a nonempty_list = ( :: ) of ('a * 'a list)

  val opt_multi : string nonempty_list -> 'a conv -> 'a list t
  val opt : string nonempty_list -> 'a conv -> 'a option t
  val opt_req : string nonempty_list -> 'a conv -> 'a t
  val flag_count : string nonempty_list -> int t
  val flag : string nonempty_list -> bool t
  val pos : int -> 'a conv -> 'a option t
  val pos_req : int -> 'a conv -> 'a t
  val pos_all : 'a conv -> 'a list t
end

module Command : sig
  type 'a t

  val singleton : 'a Term.t -> 'a t
  val group : ?default_term:'a Term.t -> (string * 'a t) list -> 'a t
  val run : 'a t -> 'a
end
