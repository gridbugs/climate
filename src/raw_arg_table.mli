open! Import

(** This data structure holds all the raw arguments parsed from the
    command line before any conversions are applied, along with the
    spec that they were parsed under. *)
type t

(** [parse spec args ~ignore_errors] parses the command-line arguments
    [args] using [spec], returning the raw argument table, or the
    appropriate [Error.Parse_error.t] if the arguments were invalid
    according to the given spec. Retains the reference to the spec for
    further error checking when querying the table.

    If [ignore_errors] is [true] then this is guaranteed to return
    [Ok _], ignoring all parse errors. This is intended for use when
    parsing partial commands lines, such as during reentrant
    completion. *)
val parse : Spec.t -> string list -> ignore_errors:bool -> (t, Error.Parse_error.t) result

(** [get_opts_names_by_name t names] returns a list of [Name.t *
    string] tuples associating values with the corresponding argument
    names from the command line. This is to help with error messages
    since a single argument may have multiple names and we want to
    print the specific name used to pass errorneous argument values in
    error messages.

    Raises if any name in [names] is not the name of an argument known
    by the [Spec.t] used to construct [t], or if any name in [names]
    is is known by the [Spec.t] but does not take a value. *)
val get_opts_names_by_name : t -> Name.t Nonempty_list.t -> (Name.t * string) list

(** [get_flag_count_names t names] returns the count of the total
    number of times that any name in [names] appeared on the command
    line.

    Raises if any name in [names] is not the name of an argument in
    the [Spec.t] used to construct [t], or if ane name in [names] is
    an argument with a value according to the [Spec.t] used to
    construct [t]. (This function is intended for counting flags
    only.) *)
val get_flag_count_names : t -> Name.t Nonempty_list.t -> int

(** [get_pos t n] returns the nth positional argument that was passed. *)
val get_pos : t -> int -> string option

(** [get_pos_all t] returns all positional arguments that were passed. *)
val get_pos_all : t -> string list
