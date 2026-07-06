(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

open Import

let hash_combine h accu = (accu * 65599) + h

module Ids : sig
  module Id : sig
    type t

    val equal : t -> t -> bool
    val zero : t
    val hash : t -> int
    val pp : t Fmt.t

    module Hash_set : sig
      type id := t
      type t

      val create : unit -> t
      val mem : t -> id -> bool
      val add : t -> id -> unit
      val clear : t -> unit
    end
  end

  type t

  val create : unit -> t
  val next : t -> Id.t
end = struct
  module Id = struct
    type t = int

    module Hash_set = Hash_set

    let equal = Int.equal
    let zero = 0
    let hash x = x
    let pp = Fmt.int
  end

  type t = int ref

  let create () = ref 0

  let next t =
    incr t;
    !t
  ;;
end

module Int_map = struct
  include Map.Make (Int)

  let to_dyn a_to_dyn t =
    bindings t
    |> List.map ~f:(fun (k, a) -> Dyn.pair (Dyn.int k) (a_to_dyn a))
    |> Dyn.list
  ;;
end

module Id = Ids.Id

module Sem = struct
  type t =
    [ `Longest
    | `Shortest
    | `First
    ]

  let to_string = function
    | `Shortest -> "short"
    | `Longest -> "long"
    | `First -> "first"
  ;;

  let to_string_short = function
    | `Shortest -> "S"
    | `Longest -> "L"
    | `First -> "F"
  ;;

  let hash t acc =
    match t with
    | `Longest -> hash_combine 0 acc
    | `Shortest -> hash_combine 1 acc
    | `First -> hash_combine 2 acc
  ;;

  let to_dyn t = Dyn.enum (to_string t)
  let equal = Poly.equal
  let pp ch k = Format.pp_print_string ch (to_string k)
end

module Rep_kind = struct
  type t =
    [ `Greedy
    | `Non_greedy
    ]

  let to_string = function
    | `Greedy -> "Greedy"
    | `Non_greedy -> "Non_greedy"
  ;;

  let to_string_short = function
    | `Greedy -> "G"
    | `Non_greedy -> "N"
  ;;

  let hash t acc =
    match t with
    | `Greedy -> hash_combine 0 acc
    | `Non_greedy -> hash_combine 1 acc
  ;;

  let to_dyn t = Dyn.enum (to_string t)
  let equal = Poly.equal
  let pp fmt t = Format.pp_print_string fmt (to_string t)
end

module Mark : sig
  type t = private int

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int -> int
  val pp : t Fmt.t
  val to_dyn : t -> Dyn.t
  val start : t
  val prev : t -> t
  val next : t -> t
  val next2 : t -> t
  val group_count : t -> int
  val outside_range : t -> start_inclusive:t -> stop_inclusive:t -> bool
end = struct
  type t = int

  let equal = Int.equal
  let compare = Int.compare
  let hash t acc = hash_combine t acc
  let pp = Format.pp_print_int
  let to_dyn = Dyn.int
  let start = 0
  let prev x = pred x
  let next x = succ x
  let next2 x = x + 2
  let group_count x = x / 2

  let outside_range t ~start_inclusive ~stop_inclusive =
    t < start_inclusive || t > stop_inclusive
  ;;
end

module Idx : sig
  type t = private int

  val pp : t Fmt.t
  val to_dyn : t -> Dyn.t
  val to_int : t -> int
  val unknown : t
  val initial : t
  val used : t -> bool
  val make : int -> t
  val equal : t -> t -> bool
end = struct
  type t = int

  let to_dyn = Dyn.int
  let to_int x = x
  let pp = Format.pp_print_int
  let used t = t >= 0
  let make x = x
  let equal = Int.equal
  let unknown = -1
  let initial = 0
end

module Pos_or_neg = struct
  type t =
    | Pos
    | Neg

  let to_dyn = function
    | Pos -> Dyn.variant "Pos" []
    | Neg -> Dyn.variant "Neg" []
  ;;

  let pp ch = function
    | Pos -> Format.fprintf ch "pos"
    | Neg -> Format.fprintf ch "neg"
  ;;

  let equal t1 t2 =
    match t1, t2 with
    | Pos, Pos | Neg, Neg -> true
    | (Pos | Neg), _ -> false
  ;;

  let hash t acc =
    match t with
    | Pos -> hash_combine 0 acc
    | Neg -> hash_combine 1 acc
  ;;

  let apply t b =
    match t with
    | Pos -> b
    | Neg -> not b
  ;;
end

module Expr = struct
  type t =
    { id : Id.t
    ; def : def
    }

  and def =
    | Cst of Cset.t
    | Alt of t list
    | Seq of Sem.t * t * t
      (* In Seq (sem, t1, t2), sem describes which match of t1 is
         preferred, but says nothing about t2 *)
    | Eps
    | Rep of Rep_kind.t * Sem.t * t
    | Mark of Mark.t
    | Erase of Mark.t * Mark.t
    | Before of Category.t
    | After of Category.t
    | Pmark of Pmark.t
    | Lookahead of Pos_or_neg.t * t
    | Lookbehind of Pos_or_neg.t * t * int

  let rec seq_as_list sem t =
    match t.def with
    | Eps -> []
    | Seq (sem', x, y) when Sem.equal sem sem' -> x :: seq_as_list sem y
    | _ -> [ t ]
  ;;

  let sem_kind_suffix ?kind sem =
    ":"
    ^ (match kind with
       | None -> ""
       | Some k -> Rep_kind.to_string_short k)
    ^ Sem.to_string_short sem
  ;;

  let rec dyn_of_def =
    let open Dyn in
    function
    | Cst cset -> Cset.to_dyn cset
    | Alt alt -> variant "Alt" (List.map ~f:to_dyn alt)
    | Seq (sem, x, y) ->
      let y = seq_as_list sem y in
      variant ("Seq" ^ sem_kind_suffix sem) (to_dyn x :: List.map y ~f:to_dyn)
    | Eps -> Enum "Eps"
    | Rep (kind, sem, t) -> variant ("Rep" ^ sem_kind_suffix ~kind sem) [ to_dyn t ]
    | Mark m -> variant "Mark" [ Mark.to_dyn m ]
    | Pmark m -> variant "Pmark" [ Pmark.to_dyn m ]
    | Erase (x, y) -> variant "Erase" [ Mark.to_dyn x; Mark.to_dyn y ]
    | Before c -> variant "Before" [ Category.to_dyn c ]
    | After c -> variant "After" [ Category.to_dyn c ]
    | Lookahead (pn, x) -> variant "Lookahead" [ Pos_or_neg.to_dyn pn; to_dyn x ]
    | Lookbehind (pn, x, id) ->
      variant "Lookbehind" [ Pos_or_neg.to_dyn pn; to_dyn x; Dyn.int id ]

  and to_dyn { id = _; def } = dyn_of_def def

  let rec pp ch e =
    let open Fmt in
    match e.def with
    | Cst l -> sexp ch "cst" Cset.pp l
    | Alt l -> sexp ch "alt" (list pp) l
    | Seq (k, e, e') -> sexp ch "seq" (triple Sem.pp pp pp) (k, e, e')
    | Eps -> str ch "eps"
    | Rep (rk, k, e) -> sexp ch "rep" (triple Rep_kind.pp Sem.pp pp) (rk, k, e)
    | Mark i -> sexp ch "mark" Mark.pp i
    | Pmark i -> sexp ch "pmark" Pmark.pp i
    | Erase (b, e) -> sexp ch "erase" (pair Mark.pp Mark.pp) (b, e)
    | Before c -> sexp ch "before" Category.pp c
    | After c -> sexp ch "after" Category.pp c
    | Lookahead (pn, e) -> sexp ch "lookahead" (pair Pos_or_neg.pp pp) (pn, e)
    | Lookbehind (pn, e, id) ->
      sexp ch "lookbehind" (triple Pos_or_neg.pp pp int) (pn, e, id)
  ;;

  let eps_expr = { id = Id.zero; def = Eps }
  let mk ids def = { id = Ids.next ids; def }
  let empty ids = mk ids (Alt [])
  let cst ids s = if Cset.is_empty s then empty ids else mk ids (Cst s)
  let eps ids = mk ids Eps
  let rep ids kind sem x = mk ids (Rep (kind, sem, x))
  let mark ids m = mk ids (Mark m)
  let pmark ids i = mk ids (Pmark i)
  let erase ids m m' = mk ids (Erase (m, m'))
  let before ids c = mk ids (Before c)
  let after ids c = mk ids (After c)
  let lookahead ids pn e = mk ids (Lookahead (pn, e))

  let alt ids = function
    | [] -> empty ids
    | [ c ] -> c
    | l -> mk ids (Alt l)
  ;;

  let seq ids (kind : Sem.t) x y =
    match x.def, y.def with
    | Alt [], _ -> x
    | _, Alt [] -> y
    | Eps, _ -> y
    | _, Eps when Sem.equal kind `First -> x
    | _ -> mk ids (Seq (kind, x, y))
  ;;

  let lookbehind ids pn e = mk ids (Lookbehind (pn, e, -1))

  let is_eps expr =
    match expr.def with
    | Eps -> true
    | _ -> false
  ;;

  let rec rename ids x =
    match x.def with
    | Cst _ | Eps | Mark _ | Pmark _ | Erase _ | Before _ | After _ -> mk ids x.def
    | Alt l -> mk ids (Alt (List.map ~f:(rename ids) l))
    | Seq (k, y, z) -> mk ids (Seq (k, rename ids y, rename ids z))
    | Rep (g, k, y) -> mk ids (Rep (g, k, rename ids y))
    | Lookahead (pn, e) -> mk ids (Lookahead (pn, rename ids e))
    | Lookbehind (pn, e, id) -> mk ids (Lookbehind (pn, rename ids e, id))
  ;;

  let rec min_size cache t =
    match Hashtbl.find_opt cache t.id with
    | Some size -> size
    | None ->
      let size =
        match t.def with
        | Cst _ -> 1
        | Alt ts ->
          List.fold_left ~init:0 ts ~f:(fun acc t -> Int.min acc (min_size cache t))
        | Seq (_, t1, t2) -> min_size cache t1 + min_size cache t2
        | Rep _
        | Eps
        | Mark _
        | Erase _
        | Before _
        | After _
        | Lookahead _
        | Lookbehind _
        | Pmark _ -> 0
      in
      Hashtbl.add cache t.id size;
      size
  ;;

  module Int_inf = struct
    type t = int option (* None = +infinity *)

    let max (t1 : t) t2 =
      match t1, t2 with
      | None, _ | _, None -> None
      | Some i1, Some i2 -> Some (Int.max i1 i2)
    ;;

    let plus t1 t2 =
      match t1, t2 with
      | None, _ | _, None -> None
      | Some i1, Some i2 -> Some (i1 + i2)
    ;;

    let minus_saturating t1 i2 =
      match t1 with
      | None | Some 0 -> t1
      | Some i1 -> Some (Int.max 0 (i1 - i2 ()))
    ;;
  end

  let rec max_size t =
    match t.def with
    | Cst _ -> Some 1
    | Alt ts ->
      List.fold_left ~init:(Some 0) ts ~f:(fun acc t -> Int_inf.max acc (max_size t))
    | Seq (_, t1, t2) -> Int_inf.plus (max_size t1) (max_size t2)
    | Rep _ -> None
    | Eps | Mark _ | Erase _ | Before _ | After _ | Lookahead _ | Lookbehind _ | Pmark _
      -> Some 0
  ;;

  module Expr_table = Hashtbl.Make (struct
      type nonrec t = t

      let rec hash { id = _; def } acc =
        match def with
        | Cst cset -> hash_combine 0 (hash_combine (Cset.hash cset) acc)
        | Alt ts ->
          hash_combine 1 (List.fold_left ts ~init:acc ~f:(fun acc t -> hash t acc))
        | Seq (sem, t1, t2) -> hash_combine 2 (Sem.hash sem (hash t1 (hash t2 acc)))
        | Eps -> hash_combine 3 acc
        | Rep (rk, sem, t1) ->
          hash_combine 4 (Rep_kind.hash rk (Sem.hash sem (hash t1 acc)))
        | Mark mark -> hash_combine 5 (Mark.hash mark acc)
        | Erase (mark1, mark2) -> hash_combine 6 (Mark.hash mark1 (Mark.hash mark2 acc))
        | Before cat -> hash_combine 7 (hash_combine (Category.to_int cat) acc)
        | After cat -> hash_combine 8 (hash_combine (Category.to_int cat) acc)
        | Pmark pmark -> hash_combine 9 (hash_combine (pmark :> int) acc)
        | Lookahead (pn, t) -> hash_combine 10 (Pos_or_neg.hash pn (hash t acc))
        | Lookbehind (pn, _, id) ->
          assert (id >= 0);
          (* Here we don't recurse in the payload to avoid quadratic complexity, in a
             pathological case where lookbehinds are nested to a depth of n. We only
             rely on the id, which is known to be initialized because collect_lookbehinds
             recurses before consulting the hashtbl. *)
          hash_combine 11 (Pos_or_neg.hash pn (hash_combine id acc))
      ;;

      let hash t = hash t 123

      let rec equal { id = _; def = def1 } { id = _; def = def2 } =
        match def1, def2 with
        | Cst cset1, Cst cset2 -> Cset.equal cset1 cset2
        | Alt t1, Alt t2 -> List.equal ~eq:equal t1 t2
        | Seq (sem1, l1, r1), Seq (sem2, l2, r2) ->
          Sem.equal sem1 sem2 && equal l1 l2 && equal r1 r2
        | Eps, Eps -> true
        | Rep (rk1, sem1, t1), Rep (rk2, sem2, t2) ->
          Rep_kind.equal rk1 rk2 && Sem.equal sem1 sem2 && equal t1 t2
        | Mark m1, Mark m2 -> Mark.equal m1 m2
        | Erase (s1, e1), Erase (s2, e2) -> Mark.equal s1 s2 && Mark.equal e1 e2
        | Before cat1, Before cat2 -> Category.equal cat1 cat2
        | After cat1, After cat2 -> Category.equal cat1 cat2
        | Pmark p1, Pmark p2 -> Pmark.equal p1 p2
        | Lookahead (pn1, t1), Lookahead (pn2, t2) ->
          Pos_or_neg.equal pn1 pn2 && equal t1 t2
        | Lookbehind (pn1, _, id1), Lookbehind (pn2, _, id2) ->
          assert (id1 >= 0 && id2 >= 0) (* same remark as in hash *);
          Pos_or_neg.equal pn1 pn2 && Int.equal id1 id2
        | ( ( Cst _
            | Alt _
            | Seq _
            | Eps
            | Rep _
            | Mark _
            | Erase _
            | Before _
            | After _
            | Pmark _
            | Lookahead _
            | Lookbehind _ )
          , _ ) -> false
      ;;
    end)

  let collect_lookbehinds t =
    (* We could try to compute something more precise, so that we don't need to run
       lookbehinds in parallel with the main regular expression constantly, but only
       when we could need them. But the naive thing has a straighforward linear cost in
       the size of the regex during compilation and when deriving every step, whereas the
       more precise computation is not that simple, especially if we have to ensure we
       don't have quadratic complexities. *)
    let min_size_cache = Hashtbl.create 7 in
    let id_by_lookbehind_expr = Expr_table.create 3 in
    let lookbehinds = ref Int_map.empty in
    let rec loop t =
      match t.def with
      | Cst _ -> Some 0, t
      | Alt ts ->
        let excess, ts =
          List.fold_left_map ts ~init:(Some 0) ~f:(fun excess t1 ->
            let excess1, t1 = loop t1 in
            Int_inf.max excess excess1, t1)
        in
        excess, { t with def = Alt ts }
      | Seq (a, t1, t2) ->
        (* We wouldn't need the cache if we returned both min_size and max_lookbehind
           from recursive calls, but the code would be less readable. Or if we knew
           that seq are never nested left, then the quadratic complexity wouldn't be
           possible in the first place. *)
        let excess1, t1 = loop t1 in
        let excess2, t2 = loop t2 in
        ( Int_inf.max
            excess1
            (Int_inf.minus_saturating excess2 (fun () -> min_size min_size_cache t1))
        , { t with def = Seq (a, t1, t2) } )
      | Rep (a, b, t1) ->
        let excess1, t1 = loop t1 in
        excess1, { t with def = Rep (a, b, t1) }
      | Lookahead (a, t1) ->
        let excess1, t1 = loop t1 in
        excess1, { t with def = Lookahead (a, t1) }
      | Lookbehind (a, t1, _) ->
        let excess1, t1 = loop t1 in
        let id =
          match Expr_table.find_opt id_by_lookbehind_expr t1 with
          | Some id -> id
          | None ->
            let id = Expr_table.length id_by_lookbehind_expr in
            Expr_table.add id_by_lookbehind_expr t1 id;
            lookbehinds := Int_map.add id t1 !lookbehinds;
            id
        in
        Int_inf.max (max_size t1) excess1, { t with def = Lookbehind (a, t1, id) }
      | Eps | Mark _ | Erase _ | Before _ | After _ | Pmark _ -> Some 0, t
    in
    let how_far_to_look_back, t = loop t in
    assert (Option.value how_far_to_look_back ~default:1 >= 0);
    how_far_to_look_back, !lookbehinds, t
  ;;
end

type expr = Expr.t

include Expr

module Initial_expr = struct
  type lookbehinds = expr Int_map.t

  type t =
    { how_far_to_look_back : int option
    ; lookbehinds : lookbehinds
    ; expr : expr
    }

  let to_dyn t =
    if Int_map.is_empty t.lookbehinds
    then to_dyn t.expr
    else
      Dyn.record
        [ "expr", to_dyn t.expr
        ; "how_far_to_look_back", Dyn.option Dyn.int t.how_far_to_look_back
        ; "lookbehinds", Int_map.to_dyn to_dyn t.lookbehinds
        ]
  ;;

  let create expr ids ~canycolor =
    let how_far_to_look_back, lookbehinds, expr = collect_lookbehinds expr in
    { how_far_to_look_back
    ; lookbehinds =
        Int_map.map
          (fun e ->
            (* Lookaheads are anchored on the left (to the current position), and
               unanchored on the right. Lookbehinds are the other way around, so we
               need an implied .* to unanchor it on the left, and special treatment
               in [create_state] to right anchor it to the current position.

               Compile.compile omits the .* sometimes. Not sure if we should do
               something similar *)
            seq ids `First (rep ids `Non_greedy `Shortest (cst ids canycolor)) e)
          lookbehinds
    ; expr
    }
  ;;
end

module Marks = struct
  type t =
    { marks : (Mark.t * Idx.t) list
    ; pmarks : Pmark.Set.t
    }

  let to_dyn { marks; pmarks } : Dyn.t =
    let open Dyn in
    record
      [ ( "marks"
        , List.map marks ~f:(fun (m, idx) -> pair (Mark.to_dyn m) (Idx.to_dyn idx))
          |> list )
      ; "pmarks", Pmark.Set.to_list pmarks |> List.map ~f:Pmark.to_dyn |> list
      ]
  ;;

  let equal { marks; pmarks } t =
    List.equal
      ~eq:(fun (x, y) (x', y') -> Mark.equal x x' && Idx.equal y y')
      marks
      t.marks
    && Pmark.Set.equal pmarks t.pmarks
  ;;

  let empty = { marks = []; pmarks = Pmark.Set.empty }

  let hash_marks_offset =
    let f acc ((a : Mark.t), (i : Idx.t)) =
      hash_combine (a :> int) (hash_combine (i :> int) acc)
    in
    fun l init -> List.fold_left l ~init ~f
  ;;

  let hash m accu = hash_marks_offset m.marks (hash_combine (Hashtbl.hash m.pmarks) accu)

  let marks_set_idx =
    let rec marks_set_idx idx marks =
      match marks with
      | [] -> []
      | (a, idx') :: rem ->
        if Idx.equal idx' Idx.unknown then (a, idx) :: marks_set_idx idx rem else marks
    in
    fun marks idx -> { marks with marks = marks_set_idx idx marks.marks }
  ;;

  let filter t (b : Mark.t) (e : Mark.t) =
    { t with
      marks =
        List.filter t.marks ~f:(fun ((i : Mark.t), _) ->
          Mark.outside_range i ~start_inclusive:b ~stop_inclusive:e)
    }
  ;;

  let set_mark t (i : Mark.t) =
    { t with marks = (i, Idx.unknown) :: List.remove_assq i t.marks }
  ;;

  let set_pmark t i = { t with pmarks = Pmark.Set.add i t.pmarks }

  let pp fmt { marks; pmarks } =
    Format.pp_open_box fmt 1;
    (match marks with
     | [] -> ()
     | _ :: _ ->
       Format.fprintf
         fmt
         "@[<2>marks@ %a@]"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
            (fun fmt (a, i) -> Format.fprintf fmt "%a-%a" Mark.pp a Idx.pp i))
         marks);
    (match Pmark.Set.to_list pmarks with
     | [] -> ()
     | pmarks ->
       Format.fprintf fmt "@[<2>pmarks %a@]" (Format.pp_print_list Pmark.pp) pmarks);
    Format.pp_close_box fmt ()
  ;;
end

module Status = struct
  type t =
    | Failed
    | Match of Mark_infos.t * Pmark.Set.t
    | Running
end

module Desc : sig
  type t

  val pp : t Fmt.t

  module E : sig
    type nonrec t = private
      | TSeq of Sem.t * t * Expr.t
      | TExp of Marks.t * Expr.t
      | TMatch of Marks.t
      | TSide_condition of bool * t * Pos_or_neg.t * t
    (* TSide_condition (has_match, t1, pn, t2) matches the same way as t1, but only
       when the side condition t2 also matches (when pn = `Pos) or fails to match (when
       pn = `Neg). This is a bit like an intersection, but an intersection is symmetric
       whereas t2 neither consume character not contains marks. [has_match] caches
       whether t1 has a TMatch or a TSide_condition (true, ...), so we can determine
       whether there is anything interesting without recursing every time. *)
  end

  val to_dyn : t -> Dyn.t
  val fold_right : t -> init:'acc -> f:(E.t -> 'acc -> 'acc) -> 'acc
  val tseq : Sem.t -> t -> Expr.t -> t -> t
  val tside_condition : t -> Pos_or_neg.t -> t -> t -> t
  val texp : Marks.t -> Expr.t -> t -> t
  val initial : Expr.t -> t
  val empty : t
  val set_idx : Idx.t -> t -> t
  val hash : t -> int -> int
  val equal : t -> t -> bool
  val status : t -> Status.t
  val split_at_exact_match : t -> t * t

  type quasi_match =
    [ `Match of Marks.t
    | `Side_condition_match of Marks.t * Pos_or_neg.t * t
    ]

  val all_quasi_matches_rev : t -> quasi_match list
  val first_quasi_match : t -> [ `None | `Match of Marks.t | `Side_condition_match ]
  val remove_quasi_matches : t -> t
  val split_at_quasi_matches_rev : t -> [ quasi_match | `Chunk of t ] list
  val add_match : t -> Marks.t -> t
  val add_eps : t -> Marks.t -> t
  val add_expr : t -> E.t -> t
  val iter_marks : t -> f:(Marks.t -> unit) -> unit
  val remove_duplicates : Id.Hash_set.t -> t -> Expr.t -> t
end = struct
  module E = struct
    type t =
      | TSeq of Sem.t * t list * Expr.t
      | TExp of Marks.t * Expr.t
      | TMatch of Marks.t
      | TSide_condition of bool * t list * Pos_or_neg.t * t list
    (* [t] is a slight variation of [Expr.t], so we have somewhere to store marks.

       TMatch is produced when deriving a regex succeeds without consuming the input
       character. As a result, the derivation proceeds into whatever follows the
       TMatch, which means we have the invariant that TMatch can only show up in the
       top-most list, or nested recursively on the left side of TSide_condition nodes,
       but never nested instead of a seq. Concretely, this invariant is maintained
       by having any TSeq construction drop or bubble up TMatch using remove_exact_matches
       or similar functions.
    *)

    let rec equal_list l1 l2 = List.equal ~eq:equal l1 l2

    and equal x y =
      match x, y with
      | TSeq (_, l1, e1), TSeq (_, l2, e2) -> Id.equal e1.id e2.id && equal_list l1 l2
      | TExp (marks1, e1), TExp (marks2, e2) ->
        Id.equal e1.id e2.id && Marks.equal marks1 marks2
      | TMatch marks1, TMatch marks2 -> Marks.equal marks1 marks2
      | TSide_condition (b1, main1, pn1, cond1), TSide_condition (b2, main2, pn2, cond2)
        ->
        Bool.equal b1 b2
        && Pos_or_neg.equal pn1 pn2
        && equal_list main1 main2
        && equal_list cond1 cond2
      | (TSeq _ | TExp _ | TMatch _ | TSide_condition _), _ -> false
    ;;

    let rec hash (t : t) accu =
      match t with
      | TSeq (_, l, e) ->
        hash_combine 0x172a1bce (hash_combine (Id.hash e.id) (hash_list l accu))
      | TExp (marks, e) ->
        hash_combine 0x2b4c0d77 (hash_combine (Id.hash e.id) (Marks.hash marks accu))
      | TMatch marks -> hash_combine 0x1c205ad5 (Marks.hash marks accu)
      | TSide_condition (_, l1, pn, l2) ->
        hash_combine 1 (hash_list l1 (Pos_or_neg.hash pn (hash_list l2 accu)))

    and hash_list =
      let f acc x = hash x acc in
      fun l init -> List.fold_left l ~init ~f
    ;;
  end

  type t = E.t list

  let rec to_dyn t = Dyn.list (List.map ~f:dyn_of_e t)

  and dyn_of_e =
    let open Dyn in
    function
    | E.TSeq (sem, x, y) ->
      variant ("TSeq" ^ sem_kind_suffix sem) [ to_dyn x; Expr.to_dyn y ]
    | TExp (marks, e) ->
      let e =
        let base = [ Expr.to_dyn e ] in
        if Marks.(equal empty marks) then base else Marks.to_dyn marks :: base
      in
      variant "TExp" e
    | TMatch m -> variant "TMatch" [ Marks.to_dyn m ]
    | TSide_condition (_, t1, pn, t2) ->
      variant "TSide_condition" [ to_dyn t1; Pos_or_neg.to_dyn pn; to_dyn t2 ]
  ;;

  open E

  let equal = E.equal_list
  let hash = E.hash_list

  let tseq kind x y rem =
    match x with
    | [] -> rem
    | [ TExp (marks, { def = Eps; _ }) ] -> TExp (marks, y) :: rem
    | _ -> TSeq (kind, x, y) :: rem
  ;;

  let texp marks e rem = TExp (marks, e) :: rem

  let tside_condition l1 (pn : Pos_or_neg.t) l2 rem =
    match l1 with
    | [] -> rem
    | _ :: _ ->
      let condition =
        match l2 with
        | [] -> Some false
        | _ ->
          if List.exists l2 ~f:(function
               | TMatch _ | TExp (_, { def = Eps; _ }) -> true
               | _ -> false)
          then Some true
          else None
      in
      (match condition with
       | None ->
         let has_match =
           List.exists l1 ~f:(function
             | TMatch _ -> true
             | TSide_condition (has_match, _, _, _) -> has_match
             | _ -> false)
         in
         TSide_condition (has_match, l1, pn, l2) :: rem
       | Some b -> if Pos_or_neg.apply pn b then l1 @ rem else rem)
  ;;

  let rec fold_right t ~init ~f =
    match t with
    | [] -> init
    | x :: xs -> f x (fold_right xs ~init ~f)
  ;;

  let rec iter_marks t ~f =
    List.iter t ~f:(fun (e : E.t) ->
      match e with
      | TSeq (_, l, _) -> iter_marks l ~f
      | TExp (marks, _) | TMatch marks -> f marks
      | TSide_condition (_, l1, _, _) -> iter_marks l1 ~f)
  ;;

  let rec print_state_rec ch e (y : Expr.t) =
    match e with
    | TMatch marks -> Format.fprintf ch "@[<2>(TMatch@ %a)@]" Marks.pp marks
    | TSeq (sem, l', x) ->
      Format.fprintf ch "@[<2>(TSeq@ %a@ " Sem.pp sem;
      print_state_lst ch l' x;
      Format.fprintf ch "@ %a)@]" Expr.pp x
    | TExp (marks, { def = Eps; _ }) ->
      Format.fprintf ch "@[<2>(TExp@ %a@ (%a)@ (eps))@]" Id.pp y.id Marks.pp marks
    | TExp (marks, x) ->
      Format.fprintf ch "@[<2>(TExp@ %a@ (%a)@ %a)@]" Id.pp x.id Marks.pp marks Expr.pp x
    | TSide_condition (_, l1, pn, l2) ->
      Format.fprintf ch "@[<2>(TSide_condition@ ";
      print_state_lst ch l1 y;
      Format.fprintf ch "@ %a @ " Pos_or_neg.pp pn;
      print_state_lst ch l2 y;
      Format.fprintf ch ")@]"

  and print_state_lst ch l y =
    match l with
    | [] -> Format.fprintf ch "()"
    | e :: rem ->
      print_state_rec ch e y;
      List.iter rem ~f:(fun e ->
        Format.fprintf ch "@ | ";
        print_state_rec ch e y)
  ;;

  let pp ch t = print_state_lst ch [ t ] { id = Id.zero; def = Eps }

  let split_at_exact_match =
    let rec split_at_exact_match_rec l = function
      | [] -> assert false
      | TMatch _ :: r -> List.rev l, r
      | x :: r -> split_at_exact_match_rec (x :: l) r
    in
    fun l -> split_at_exact_match_rec [] l
  ;;

  let rec first_quasi_match = function
    | [] -> `None
    | TMatch marks :: _ -> `Match marks
    | TSide_condition (true, _, _, _) :: _ -> `Side_condition_match
    | _ :: rem -> first_quasi_match rem
  ;;

  let flatten_side_condition
    (inner_pn : Pos_or_neg.t)
    (inner_cond : t list)
    (outer_pn : Pos_or_neg.t)
    outer_cond
    : Pos_or_neg.t * t list
    =
    (*
       ((e assuming a) assuming b) => (e assuming (a assuming b))
       ((e assuming not a) assuming b) => (e assuming (b assuming not a))
       ((e assuming a) assuming not b) => (e assuming (a assuming not b))
       ((e assuming not a) assuming not b) => (e assuming not (a or b))
       When e is a TMatch, this is useful to bubble up the TMatch during derivation.
    *)
    match inner_pn, outer_pn with
    | Pos, (Pos | Neg) -> inner_pn, tside_condition inner_cond outer_pn outer_cond []
    | Neg, Pos -> outer_pn, tside_condition outer_cond inner_pn inner_cond []
    | Neg, Neg -> Neg, inner_cond @ outer_cond
  ;;

  type quasi_match =
    [ `Match of Marks.t
    | `Side_condition_match of Marks.t * Pos_or_neg.t * t list
    ]

  let rec all_quasi_matches_rev acc = function
    | [] -> acc
    | TMatch marks :: _ -> `Match marks :: acc
    | TSide_condition (true, t1, pn, t2) :: tl ->
      (* here I went the way of supporting nested side_conditions. But another
         possibility would have been, when deriving TSide_conditions, to lift
         out any inner TSide_condition. That would create duplication of
         the side condition on either side, which means we may want to merge
         them afterwards. In some sense, the deep version here might be the
         equivalent of having ropes in the ast. *)
      let sub_matches_rev =
        all_quasi_matches_rev [] t1
        |> List.map ~f:(function
          | `Match marks -> `Side_condition_match (marks, pn, t2)
          | `Side_condition_match (marks, pn', t2') ->
            let pn_final, t2_final = flatten_side_condition pn' t2' pn t2 in
            `Side_condition_match (marks, pn_final, t2_final))
      in
      all_quasi_matches_rev (sub_matches_rev @ acc) tl
    | _ :: tl -> all_quasi_matches_rev acc tl
  ;;

  let all_quasi_matches_rev t = all_quasi_matches_rev [] t

  let[@tail_mod_cons] rec remove_quasi_matches = function
    | [] -> []
    | TMatch _ :: rest -> remove_quasi_matches rest
    | TSide_condition (true, t1, pn, t2) :: rest ->
      (match remove_quasi_matches t1 with
       | [] -> remove_quasi_matches rest
       | _ :: _ as t1 -> TSide_condition (false, t1, pn, t2) :: remove_quasi_matches rest)
    | elt :: rest -> elt :: remove_quasi_matches rest
  ;;

  (* not sure why that's worse, maybe should confirm on a more stable bench machine *)
  (* let remove_quasi_matches t = *)
  (*   List.filter_map t ~f:(function *)
  (*     | TMatch _ -> None *)
  (*     | TSide_condition (l, pn, l2) when has_exact_match l -> *)
  (*       (match remove_exact_matches l with *)
  (*        | [] -> None *)
  (*        | _ :: _ as l -> Some (TSide_condition (l, pn, l2))) *)
  (*     | elt -> Some elt) *)
  (* ;; *)

  let split_at_quasi_matches_rev =
    let rec loop acc chunk = function
      | [] -> if List.is_empty chunk then acc else `Chunk (List.rev chunk) :: acc
      | TMatch marks :: rest ->
        let acc = if List.is_empty chunk then acc else `Chunk (List.rev chunk) :: acc in
        let acc = `Match marks :: acc in
        let rest = remove_quasi_matches rest in
        if List.is_empty rest then acc else `Chunk rest :: acc
      | TSide_condition (true, l1, pn, l2) :: rest ->
        let subchunks =
          loop [] [] l1
          |> List.rev_map ~f:(function
            | `Chunk l1 -> `Chunk [ TSide_condition (false, l1, pn, l2) ]
            | `Match marks -> `Side_condition_match (marks, pn, l2)
            | `Side_condition_match (marks, pn', l2') ->
              let pn_final, t2_final = flatten_side_condition pn' l2' pn l2 in
              `Side_condition_match (marks, pn_final, t2_final))
        in
        let subchunks =
          if List.is_empty chunk
          then subchunks
          else (
            match subchunks with
            | `Chunk l :: rest -> `Chunk (List.rev_append chunk l) :: rest
            | rest -> `Chunk (List.rev chunk) :: rest)
        in
        let subchunks_rev, chunk =
          match List.rev subchunks with
          | `Chunk l :: rest -> rest, List.rev l
          | rest -> rest, []
        in
        loop (subchunks_rev @ acc) chunk rest
      | elt :: rest -> loop acc (elt :: chunk) rest
    in
    fun t -> loop [] [] t
  ;;

  let status : _ -> Status.t = function
    | [] -> Failed
    | TMatch m :: _ -> Match (Mark_infos.make (m.marks :> (int * int) list), m.pmarks)
    | _ -> Running
  ;;

  let set_idx =
    let rec f idx = function
      | TMatch marks -> TMatch (Marks.marks_set_idx marks idx)
      | TSeq (kind, l, x) -> TSeq (kind, set_idx idx l, x)
      | TExp (marks, x) -> TExp (Marks.marks_set_idx marks idx, x)
      | TSide_condition (n, l1, pn, l2) -> TSide_condition (n, set_idx idx l1, pn, l2)
    and set_idx idx xs = List.map xs ~f:(f idx) in
    set_idx
  ;;

  let[@ocaml.warning "-32"] pp fmt t =
    Format.fprintf
      fmt
      "[%a]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp)
      t
  ;;

  let empty = []
  let initial expr = [ TExp (Marks.empty, expr) ]
  let add_match t marks = TMatch marks :: t
  let add_eps t marks = TExp (marks, eps_expr) :: t
  let add_expr t expr = expr :: t

  let remove_duplicates =
    let rec loop seen l y =
      match l with
      | [] -> []
      | (TMatch _ as x) :: _ ->
        (* Truncate after first match *)
        [ x ]
      | TSeq (kind, l, x) :: r ->
        let l = loop seen l x in
        let r = loop seen r y in
        tseq kind l x r
      | (TExp (_marks, { def = Eps; _ }) as e) :: r ->
        if Id.Hash_set.mem seen y.id
        then loop seen r y
        else (
          Id.Hash_set.add seen y.id;
          e :: loop seen r y)
      | (TExp (_marks, x) as e) :: r ->
        if Id.Hash_set.mem seen x.id
        then loop seen r y
        else (
          Id.Hash_set.add seen x.id;
          e :: loop seen r y)
      | TSide_condition (_, l1, pn, l2) :: r when true (* XXX unlikely to be ideal!! *) ->
        tside_condition l1 pn l2 r
      | TSide_condition (_, l1, pn, l2) :: r ->
        let l1 = loop seen l1 y in
        let l2 = loop seen l2 y in
        let r = loop seen r y in
        tside_condition l1 pn l2 r
    in
    fun seen l y ->
      Id.Hash_set.clear seen;
      loop seen l y
  ;;
end

module E = Desc.E

module State = struct
  type t =
    { idx : Idx.t
    ; category : Category.t
    ; lookbehinds : Desc.t Int_map.t
    ; before_start : bool
        (* Whether we are matching before Re.start. This happens when lookbehinds
           need information before the start of matches. When the flag is set,
           we feed characters into the lookbehinds, but not the main desc. *)
    ; desc : Desc.t
    ; mutable status : Status.t option
    ; hash : int
    }
  (* Thread-safety: We use double-checked locking to access field
     [status] in function [status] below. *)

  let pp fmt t = Desc.pp fmt t.desc
  let[@inline] idx t = t.idx

  let to_dyn t =
    if Int_map.is_empty t.lookbehinds && not t.before_start
    then Desc.to_dyn t.desc
    else
      Dyn.record
        [ "desc", Desc.to_dyn t.desc
        ; "lookbehinds", Int_map.to_dyn Desc.to_dyn t.lookbehinds
        ; "before_start", Dyn.bool t.before_start
        ]
  ;;

  let dummy =
    { idx = Idx.unknown
    ; category = Category.dummy
    ; lookbehinds = Int_map.empty
    ; before_start = false
    ; desc = Desc.empty
    ; status = None
    ; hash = -1
    }
  ;;

  let hash idx cat lookbehinds ~before_start desc =
    let hash = 0 in
    let hash = hash_combine (Category.to_int cat) hash in
    let hash = hash_combine (Bool.to_int before_start) hash in
    let hash = hash_combine idx hash in
    let hash = Desc.hash desc hash in
    let hash =
      Int_map.fold
        (fun key data acc -> hash_combine key (Desc.hash data acc))
        lookbehinds
        hash
    in
    hash land 0x3FFFFFFF
  ;;

  let mk idx cat lookbehinds ~before_start desc =
    { idx
    ; category = cat
    ; lookbehinds
    ; before_start
    ; desc
    ; status = None
    ; hash = hash (idx :> int) cat lookbehinds ~before_start desc
    }
  ;;

  let create cat (e : Initial_expr.t) =
    mk
      Idx.initial
      cat
      (Int_map.map Desc.initial e.lookbehinds)
      (* Even if we start at beginning of string, and thus we can't look further back,
         Compile.match_before_start will call advance (`At_match_start ..) to unset that
         flag. *)
      ~before_start:(Option.value e.how_far_to_look_back ~default:1 > 0)
      (Desc.initial e.expr)
  ;;

  let equal { idx; category; lookbehinds; before_start; desc; status = _; hash } t =
    Int.equal hash t.hash
    && Idx.equal idx t.idx
    && Category.equal category t.category
    && Bool.equal before_start t.before_start
    && Desc.equal desc t.desc
    && Int_map.equal Desc.equal lookbehinds t.lookbehinds
  ;;

  (* To be called when the mutex has already been acquired *)
  let status_no_mutex s =
    match s.status with
    | Some s -> s
    | None ->
      let st = Desc.status s.desc in
      s.status <- Some st;
      st
  ;;

  let status m s =
    match s.status with
    | Some s -> s
    | None ->
      Mutex.lock m;
      let st = status_no_mutex s in
      Mutex.unlock m;
      st
  ;;

  module Table = Hashtbl.Make (struct
      type nonrec t = t

      let equal = equal
      let hash t = t.hash
    end)
end

(**** Find a free index ****)

module Working_area = struct
  type t =
    { mutable ids : Bit_vector.t
    ; seen : Id.Hash_set.t
    ; index_count : int Atomic.t
    }

  let create () =
    { ids = Bit_vector.create_zero 1
    ; seen = Id.Hash_set.create ()
    ; index_count = Atomic.make 0
    }
  ;;

  let index_count w = Atomic.get w.index_count

  let mark_used_indices tbl =
    Desc.iter_marks ~f:(fun marks ->
      List.iter marks.marks ~f:(fun (_, i) ->
        if Idx.used i then Bit_vector.set tbl (i :> int) true))
  ;;

  let rec find_free tbl idx len =
    if idx = len || not (Bit_vector.get tbl idx) then idx else find_free tbl (idx + 1) len
  ;;

  let free_index t l =
    Bit_vector.reset_zero t.ids;
    mark_used_indices t.ids l;
    let len = Bit_vector.length t.ids in
    let idx = find_free t.ids 0 len in
    if idx = len
    then (
      t.ids <- Bit_vector.create_zero (2 * len);
      (* This function is only called when the mutex is locked. So we
         are sure that this is always coherent with the length of
         [t.ids]. *)
      Atomic.set t.index_count (2 * len));
    Idx.make idx
  ;;
end

(**** Computation of the next state ****)

type ctx =
  | Delta of
      { c : Cset.c
      ; prev_cat : Category.t
      ; next_cat : Category.t
      ; lookbehinds : (Desc.t * Desc.quasi_match list Lazy.t) Int_map.t ref
      }
  | Advance of
      { prev_cat : Category.t
      ; how : [ `Partial | `At_match_stop of Category.t ]
      ; lookbehinds : (Desc.t * Desc.quasi_match list Lazy.t) Int_map.t ref
      }

let rec delta_expr ctx marks (x : Expr.t) rem =
  (*Format.eprintf "%d@." x.id;*)
  match x.def with
  | Cst s ->
    (match ctx with
     | Delta { c; _ } -> if Cset.mem c s then Desc.add_eps rem marks else rem
     | Advance _ -> Desc.texp marks x rem)
  | Alt l -> delta_alt ctx marks l rem
  | Seq (kind, y, z) ->
    let y = delta_expr ctx marks y Desc.empty in
    delta_seq ctx kind y z rem
  | Rep (rep_kind, kind, y) -> delta_rep ctx marks x rep_kind kind y rem
  | Eps -> Desc.add_match rem marks
  | Mark i -> Desc.add_match rem (Marks.set_mark marks i)
  | Pmark i -> Desc.add_match rem (Marks.set_pmark marks i)
  | Erase (b, e) -> Desc.add_match rem (Marks.filter marks b e)
  | Before cat ->
    (match ctx with
     | Delta { next_cat; _ } | Advance { how = `At_match_stop next_cat; _ } ->
       if Category.intersect next_cat cat then Desc.add_match rem marks else rem
     | Advance { how = `Partial; _ } ->
       (* We can't simply return Desc.texp, because in the Advance case, we need to
          preserve the invariant that if some regex could be a Match, then it must be
          a Match. If we don't, then we can end up with
          Seq `Shortest [ could_be_a_match; match ] rest, which would become
          [ rest (with the bindings from match); Seq `Shortest could_be_a_match rest ],
          ie we reordered the two branches incorrectly. *)
       Desc.tside_condition
         (Desc.add_match Desc.empty marks)
         Pos
         (Desc.texp marks x Desc.empty)
         rem)
  | After cat ->
    let prev_cat =
      match ctx with
      | Delta { prev_cat; _ } -> prev_cat
      | Advance { prev_cat; _ } -> prev_cat
    in
    if Category.intersect prev_cat cat then Desc.add_match rem marks else rem
  | Lookahead (pn, e) ->
    Desc.tside_condition
      (Desc.add_match Desc.empty marks)
      pn
      (delta_expr ctx Marks.empty e Desc.empty)
      rem
  | Lookbehind (pn, _, id) ->
    let lookbehinds =
      match ctx with
      | Delta { lookbehinds; _ } -> lookbehinds
      | Advance { lookbehinds; _ } -> lookbehinds
    in
    let _, (lazy quasi_matches_rev) = Int_map.find id !lookbehinds in
    Desc.tside_condition
      (Desc.add_match Desc.empty marks)
      pn
      (List.fold_left quasi_matches_rev ~init:Desc.empty ~f:(fun acc ->
           function
           | `Match _ -> Desc.add_match acc marks
           | `Side_condition_match (_, pn', l2) ->
             Desc.tside_condition (Desc.add_match Desc.empty marks) pn' l2 acc))
      rem

and delta_rep ctx marks x rep_kind kind y rem =
  match rep_kind with
  | `Non_greedy ->
    let y = Desc.remove_quasi_matches (delta_expr ctx marks y Desc.empty) in
    Desc.add_match (Desc.tseq kind y x rem) marks
  | `Greedy ->
    let y = delta_expr ctx marks y Desc.empty in
    let matches_rev = Desc.all_quasi_matches_rev y in
    let non_matches =
      match matches_rev with
      | [] -> y
      | _ :: _ -> Desc.remove_quasi_matches y
    in
    let rem =
      match matches_rev with
      | `Match _ :: _ -> rem
      | _ -> Desc.add_match rem marks
    in
    let rem =
      List.fold_left ~init:rem matches_rev ~f:(fun rem ->
          function
          | `Match marks -> Desc.add_match rem marks
          | `Side_condition_match (marks, pn, l2) ->
            Desc.tside_condition (Desc.add_match Desc.empty marks) pn l2 rem)
    in
    Desc.tseq kind non_matches x rem

and delta_alt ctx marks l rem = List.fold_right l ~init:rem ~f:(delta_expr ctx marks)

and delta_seq ctx (kind : Sem.t) y z rem =
  match Desc.first_quasi_match y with
  | `None -> Desc.tseq kind y z rem
  | `Match marks ->
    (match kind with
     | `Longest ->
       Desc.tseq kind (Desc.remove_quasi_matches y) z (delta_expr ctx marks z rem)
     | `Shortest ->
       delta_expr ctx marks z (Desc.tseq kind (Desc.remove_quasi_matches y) z rem)
     | `First ->
       let y, y' = Desc.split_at_exact_match y in
       Desc.tseq
         kind
         y
         z
         (delta_expr ctx marks z (Desc.tseq kind (Desc.remove_quasi_matches y') z rem)))
  | `Side_condition_match ->
    let f rem = function
      | `Chunk t -> Desc.tseq kind t z rem
      | `Match marks -> delta_expr ctx marks z rem
      | `Side_condition_match (marks, pn, l2) ->
        Desc.tside_condition (delta_expr ctx marks z Desc.empty) pn l2 rem
    in
    (match kind with
     | (`Longest | `Shortest) as kind ->
       let matches_rev = Desc.all_quasi_matches_rev y in
       let non_matches = Desc.remove_quasi_matches y in
       (match kind with
        | `Shortest -> List.fold_left matches_rev ~init:(f rem (`Chunk non_matches)) ~f
        | `Longest -> f (List.fold_left matches_rev ~init:rem ~f) (`Chunk non_matches))
     | `First -> List.fold_left ~init:rem ~f (Desc.split_at_quasi_matches_rev y))
;;

let rec delta_e ctx (x : E.t) rem =
  match x with
  | TSeq (kind, y, z) ->
    let y = delta_desc ctx y Desc.empty in
    delta_seq ctx kind y z rem
  | TExp (marks, e) -> delta_expr ctx marks e rem
  | TMatch _ -> Desc.add_expr rem x
  | TSide_condition (_, l1, pn, l2) ->
    Desc.tside_condition
      (delta_desc ctx l1 Desc.empty)
      pn
      (delta_desc ctx l2 Desc.empty)
      rem

and delta_desc ctx (l : Desc.t) rem =
  Desc.fold_right l ~init:rem ~f:(fun y acc -> delta_e ctx y acc)
;;

let create_state (tbl_ref : Working_area.t) next_cat lookbehinds ~before_start expr =
  let lookbehinds =
    (* no need for Desc.set_idx in here, cause no groups *)
    Int_map.map
      (fun (desc, _) ->
        Desc.remove_duplicates tbl_ref.seen (Desc.remove_quasi_matches desc) Expr.eps_expr)
      lookbehinds
  in
  let idx = Working_area.free_index tbl_ref expr in
  let expr = Desc.set_idx idx expr in
  State.mk idx next_cat lookbehinds ~before_start expr
;;

let map_into_ref ~ref map f =
  Int_map.iter (fun key data -> ref := Int_map.add key (f data) !ref) map
;;

let delta_lookbehinds lookbehinds (st : State.t) ctx =
  (* Mapping in this non-standard way allows later [f] calls to look up the result of
     earlier [f] calls, which is necessary to support lookbehinds containing other
     lookbehinds. *)
  map_into_ref ~ref:lookbehinds st.lookbehinds (fun desc ->
    let desc = delta_desc ctx desc Desc.empty in
    desc, lazy (Desc.all_quasi_matches_rev desc))
;;

let delta (tbl_ref : Working_area.t) next_cat char (st : State.t) =
  let prev_cat = st.category in
  let lookbehinds = ref Int_map.empty in
  (let ctx = Delta { c = char; next_cat; prev_cat; lookbehinds } in
   delta_lookbehinds lookbehinds st ctx);
  let expr =
    if st.before_start
    then st.desc
    else (
      let ctx = Delta { c = char; next_cat; prev_cat; lookbehinds } in
      Desc.remove_duplicates
        tbl_ref.seen
        (delta_desc ctx st.desc Desc.empty)
        Expr.eps_expr)
  in
  create_state tbl_ref next_cat !lookbehinds ~before_start:st.before_start expr
;;

(* When deriving [Re.str "abc"] wrt "a" then "b" then "c", we end up with [T (marks,
   Eps)], i.e. not a match, until the next character. When matching whole strings, this
   is fine because we feed in an eos character (in final_advance) if necessary. For
   exec_partial though, it means we'd return `Prefix too conservatively. So here we
   advance through all epsilon transitions, so that we can detect a match or a mismatch
   without waiting for an extra character. *)
let advance (tbl_ref : Working_area.t) how (st : State.t) =
  match how with
  | `At_match_start ->
    (* It's a bit awkward to do a normal advance here, because if we run inside of
       exec_partial, we'll be followed by `Partial, and so the next_category would need
       to be absent. Rather than passing the information about whether we're in
       exec_partial, it's simpler to just not derive: we only need to update before_start
       and the category. This is unlike `At_match_stop next_cat, which cannot be followed
       by `Prefix, and does need to advance as next_cat is the only time stop_boundary
       is set. *)
    let expr = st.desc in
    let idx = Working_area.free_index tbl_ref expr in
    let expr = Desc.set_idx idx expr in
    State.mk
      idx
      Category.(st.category ++ start_boundary)
      st.lookbehinds
      ~before_start:false
      expr
  | (`Partial | `At_match_stop _) as how ->
    let prev_cat = st.category in
    let lookbehinds = ref Int_map.empty in
    (let ctx = Advance { prev_cat; lookbehinds; how } in
     delta_lookbehinds lookbehinds st ctx);
    let expr =
      if st.before_start
      then st.desc
      else (
        let ctx = Advance { prev_cat; lookbehinds; how } in
        Desc.remove_duplicates
          tbl_ref.seen
          (delta_desc ctx st.desc Desc.empty)
          Expr.eps_expr)
    in
    let expr =
      match how with
      | `Partial | `At_match_start _ -> expr
      | `At_match_stop _ ->
        (* At the match boundary, we only keep matches and side matches. This way, we
           know we can't create further matches, but we can keep reading more text to
           satisfy/dissatisify lookaheads. *)
        List.fold_left (Desc.all_quasi_matches_rev expr) ~init:Desc.empty ~f:(fun acc ->
            function
            | `Match marks -> Desc.add_match acc marks
            | `Side_condition_match (marks, pn, l2) ->
              Desc.tside_condition (Desc.add_match Desc.empty marks) pn l2 acc)
    in
    create_state tbl_ref st.category !lookbehinds ~before_start:st.before_start expr
;;
