(** Operations that mirror the Re interface, but matching utf8 instead of
    ascii/latin1 where relevant. *)

(** The syntax of regular expression, equivalent to [Re.t]. *)
type t

val to_re : t -> Re.t
val compile : t -> Re.re

(** [any] matches any valid codepoint, with a similar role to [Re.any] *)
val any : t

val str : string -> t
val alt : t list -> t
val seq : t list -> t
val empty : t
val epsilon : t
val repn : t -> int -> int option -> t
val rep : t -> t
val rep1 : t -> t
val opt : t -> t
val bol : t
val eol : t
val bos : t
val eos : t
val leol : t
val start : t
val stop : t
val whole_string : t -> t
val longest : t -> t
val shortest : t -> t
val first : t -> t
val greedy : t -> t
val non_greedy : t -> t
val group : ?name:string -> t -> t
val no_group : t -> t
val nest : t -> t
val case : t -> t

type foldcase_data

(** Pass in [Re_utf8.No_case.data] as the first argument, or call
    [Re_utf8.No_case.re]. The interface is this way so the data is not included
    into executables which do not use this functionality.

    Currently, this supports only simple case matches.

    Unicode character classes (meaning the Uchar_set.t provided in their own modules,
    like [Re_utf.Gc.Letter.set]) do not support case-insensitive matching.
    If they appear under a case-insensitive match, [to_re] will raise an exception. *)
val no_case : foldcase_data -> t -> t

(** Character sets *)

module Uchar_set : sig
  (** The type represent a fragment of regular expression matching only a single
      uchar. *)
  type t
end

val cset : Uchar_set.t -> t

(** [set] is the equivalent of [Re.set], but matching any uchar from the given
    string (rather than any byte in the given string). Beware that the uchar
    e-with-acute-accent and the uchar e followed by the uchar combining-acute-accent
    are treated differently even though they are normally visually identical.

    @raise Invalid_argument if the string is not valid utf8 *)
val set : string -> Uchar_set.t

val inter : Uchar_set.t list -> Uchar_set.t
val diff : Uchar_set.t -> Uchar_set.t -> Uchar_set.t
val compl : Uchar_set.t list -> Uchar_set.t
val union : Uchar_set.t list -> Uchar_set.t
val char : Uchar.t -> Uchar_set.t
val rg : Uchar.t -> Uchar.t -> Uchar_set.t

(** Predefined ASCII sets. *)

val digit : Uchar_set.t
val notnl : Uchar_set.t
val lower : Uchar_set.t
val upper : Uchar_set.t
val alpha : Uchar_set.t
val alnum : Uchar_set.t
val wordc : Uchar_set.t
val cntrl : Uchar_set.t
val graph : Uchar_set.t
val print : Uchar_set.t
val ascii : Uchar_set.t
val blank : Uchar_set.t
val xdigit : Uchar_set.t
val punct : Uchar_set.t

(**/**)

val class_ : Re.t Lazy.t -> Uchar_set.t
val to_set_for_tests : Uchar_set.t -> Set.Make(Uchar).t
val create_foldcase_data : (int * int * int) array -> foldcase_data
