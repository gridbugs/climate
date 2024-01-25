module Term : sig
  type 'a parse = string -> ('a, [ `Msg of string ]) result
  type 'a print = Format.formatter -> 'a -> unit

  (** Knows how to interpret strings on the command line as a particular type
      and how to format values of said type as strings. Define a custom [_ conv]
      value to implement a term of a custom type. *)
  type 'a conv =
    { parse : 'a parse
    ; print : 'a print
    }

  val string : string conv
  val int : int conv
  val float : float conv
  val bool : bool conv

  (** [enum values ~eq] returns a conv for a concrete set of possible values of
      type ['a]. The values and their names are given by the [values] argument and
      [eq] is used when printing values to tie a given value of type ['a] to a
      name. *)
  val enum : (string * 'a) list -> eq:('a -> 'a -> bool) -> 'a conv

  (** A parser of values of type ['a] *)
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t

  module O : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  type 'a nonempty_list = ( :: ) of ('a * 'a list)

  (** A list of at least one string that can be constructed using regular list
      syntax (ie. ["foo"; "bar"; "baz"]). These are used to specify the names
      of arguments to parsers, as each parser with named arguments must have at
      least one name. *)
  type names := string nonempty_list

  (** A named argument that may appear multiple times on the command line. *)
  val opt_multi : names -> 'a conv -> 'a list t

  (** A named argument that may appear at most once on the command line. *)
  val opt : names -> 'a conv -> 'a option t

  (** A named argument that must appear exactly once on the command line. *)
  val opt_req : names -> 'a conv -> 'a t

  (** A flag that may appear multiple times on the command line.
      Evaluates to the number of times the flag appeared. *)
  val flag_count : names -> int t

  (** A flag that may appear at most once on the command line. *)
  val flag : names -> bool t

  (** [pos i conv] declares an optional anonymous positional argument at position
      [i] (starting at 0). *)
  val pos : int -> 'a conv -> 'a option t

  (** [pos i conv] declares a required anonymous positional argument at position
      [i] (starting at 0). *)
  val pos_req : int -> 'a conv -> 'a t

  (** Parses all positional arguments. *)
  val pos_all : 'a conv -> 'a list t

  (** [pos_left i conv] parses all positional arguments at positions less than
      i. *)
  val pos_left : int -> 'a conv -> 'a list t

  (** [pos_left i conv] parses all positional arguments at positions greater
      than or equal to i. *)
  val pos_right : int -> 'a conv -> 'a list t
end

module Command : sig
  type 'a t

  (** Declare a single command. *)
  val singleton : 'a Term.t -> 'a t

  (** [group children] returns a command with a hierarchy of subcommands, the
      leaves of which will be either singletons or empty groups (groups with an
      empty list of children). If the [default_term] argument is passed then
      sequences of subcommands may terminating with this command and will be
      passed with that argument. *)
  val group : ?default_term:'a Term.t -> (string * 'a t) list -> 'a t

  (** Run the command line parser returning its result. *)
  val run : 'a t -> 'a
end

module For_test : module type of For_test
