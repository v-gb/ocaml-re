(** Categories represent the various kinds of characters that can be tested
    by look-ahead and look-behind operations.

    This is more restricted than Cset, but faster. *)

type t [@@immediate]

val ( ++ ) : t -> t -> t
val from_char : char -> t
val dummy : t
val inexistant : t
val ascii_letter : t
val not_ascii_letter : t
val newline : t
val lastnewline : t
val search_boundary : t
val latin1_letter : t
val not_latin1_letter : t
val letter : [ `Ascii | `Latin1 ] -> t
val not_letter : [ `Ascii | `Latin1 ] -> t
val to_int : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val intersect : t -> t -> bool
val pp : t Fmt.t
val to_dyn : t -> Dyn.t
