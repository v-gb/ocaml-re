module Ascii_or_latin1 : sig
  type t =
    [ `Ascii
    | `Latin1
    ]
end

type ('a, _) ast = private
  | Alternative : 'a list -> ('a, [> `Uncased ]) ast
  | No_case : Ascii_or_latin1.t * 'a -> ('a, [> `Cased ]) ast
  | Case : 'a -> ('a, [> `Cased ]) ast

type cset = private
  | Cset of Cset.t
  | Intersection of cset list
  | Complement of cset list
  | Difference of cset * cset
  | Cast of (cset, [ `Cased | `Uncased ]) ast

type ('a, 'case) gen = private
  | Set of 'a
  | Ast of (('a, 'case) gen, 'case) ast
  | Sequence of ('a, 'case) gen list
  | Repeat of ('a, 'case) gen * int * int option
  | Lookahead of [ `Pos | `Neg ] * ('a, 'case) gen
  | Lookbehind of [ `Pos | `Neg ] * ('a, 'case) gen
  | Beg_of_line
  | End_of_line
  | Beg_of_word of Ascii_or_latin1.t
  | End_of_word of Ascii_or_latin1.t
  | Not_bound of Ascii_or_latin1.t
  | Beg_of_str
  | End_of_str
  | Last_end_of_line
  | Start
  | Stop
  | Group of string option * ('a, 'case) gen
  | No_group of ('a, 'case) gen
  | Nest of ('a, 'case) gen
  | Pmark of Pmark.t * ('a, 'case) gen
  | Sem of Automata.Sem.t * ('a, 'case) gen
  | Sem_greedy of Automata.Rep_kind.t * ('a, 'case) gen

type t = (cset, [ `Cased | `Uncased ]) gen
type no_case = (Cset.t, [ `Uncased ]) gen

val to_dyn : t -> Dyn.t
val pp : t Fmt.t
val pp_api : t Fmt.t
val merge_sequences : (Cset.t, [ `Uncased ]) gen list -> (Cset.t, [ `Uncased ]) gen list
val handle_case : t -> (Cset.t, [ `Uncased ]) gen
val anchored : t -> bool
val colorize : Color_map.t -> (Cset.t, [ `Uncased ]) gen -> bool

module Export : sig
  type nonrec t = t

  val empty : t
  val epsilon : t
  val str : string -> t
  val no_case : t -> t
  val no_case_ascii : t -> t
  val case : t -> t
  val diff : t -> t -> t
  val compl : t list -> t
  val repn : t -> int -> int option -> t
  val inter : t list -> t
  val char : char -> t
  val any : t
  val set : string -> t
  val mark : t -> Pmark.t * t
  val nest : t -> t
  val no_group : t -> t
  val whole_string : t -> t
  val leol : t
  val longest : t -> t
  val greedy : t -> t
  val non_greedy : t -> t
  val stop : t
  val not_boundary : t
  val not_boundary_ascii : t
  val group : ?name:string -> t -> t
  val word : t -> t
  val word_ascii : t -> t
  val first : t -> t
  val bos : t
  val bow : t
  val bow_ascii : t
  val eow : t
  val eow_ascii : t
  val eos : t
  val bol : t
  val start : t
  val eol : t
  val lookahead : [ `Pos | `Neg ] -> t -> t
  val lookbehind : [ `Pos | `Neg ] -> t -> t
  val opt : t -> t
  val rep : t -> t
  val rep1 : t -> t
  val alt : t list -> t
  val shortest : t -> t
  val seq : t list -> t
  val pp : t Fmt.t
  val pp_api : t Fmt.t
  val witness : t -> string
end

val cset : Cset.t -> t
val t_of_cset : cset -> t
