let lo_bound = Uchar.of_int 0xD7FF
let hi_bound = Uchar.of_int 0xE000

let rec utf8_rg a b =
  if a <= lo_bound && b >= hi_bound
  then utf8_rg a lo_bound @ utf8_rg hi_bound b
  else (
    let a = Uchar.to_int a in
    let b = Uchar.to_int b in
    (* taken from https://github.com/ocaml/ocaml-re/pull/48 *)
    let rg a b = Char.chr a, Char.chr b in
    assert (0 <= a && a <= b);
    let lo x i = 0x80 lor ((x lsr (6 * i)) land 0x3f) in
    let r : (char * char) list list ref = ref [] in
    if a <= 0x7f
    then (
      let b = min b 0x7f in
      r := [ rg a b ] :: !r);
    if a <= 0x7ff
    then (
      let b = min b 0x7ff in
      let hi x = 0xc0 lor ((x lsr 6) land 0x1f) in
      let a0 = lo a 0 in
      let a1 = hi a in
      let b0 = lo b 0 in
      let b1 = hi b in
      r := [ rg a1 b1; rg a0 b0 ] :: !r);
    if a <= 0xffff
    then (
      let b = min b 0xffff in
      let hi x = 0xe0 lor ((x lsr 12) land 0xf) in
      let a0 = lo a 0 in
      let a1 = lo a 1 in
      let a2 = hi a in
      let b0 = lo b 0 in
      let b1 = lo b 1 in
      let b2 = hi b in
      r := [ rg a2 b2; rg a1 b1; rg a0 b0 ] :: !r);
    if a <= 0x1fffff
    then (
      let b = min b 0x1fffff in
      let hi x = 0xf0 lor ((x lsr 16) land 0x7) in
      let a0 = lo a 0 in
      let a1 = lo a 1 in
      let a2 = lo a 2 in
      let a3 = hi a in
      let b0 = lo b 0 in
      let b1 = lo b 1 in
      let b2 = lo b 2 in
      let b3 = hi b in
      r := [ rg a3 b3; rg a2 b2; rg a1 b1; rg a0 b0 ] :: !r);
    !r)
;;

let any =
  let x80_to_x8f = Re.rg '\x80' '\x8f' in
  let x80_to_x9f = Re.rg '\x80' '\x9f' in
  let x80_to_xbf = Re.rg '\x80' '\xbf' in
  let x90_to_xbf = Re.rg '\x90' '\xbf' in
  let xa0_to_xbf = Re.rg '\xa0' '\xbf' in
  Re.alt
    [ Re.seq [ Re.rg '\x00' '\x7f' ]
    ; Re.seq [ Re.rg '\xc2' '\xdf'; x80_to_xbf ]
    ; Re.seq [ Re.char '\xe0'; xa0_to_xbf; x80_to_xbf ]
    ; Re.seq
        [ Re.alt [ Re.rg '\xe1' '\xec'; Re.rg '\xee' '\xef' ]; x80_to_xbf; x80_to_xbf ]
    ; Re.seq [ Re.char '\xed'; x80_to_x9f; x80_to_xbf ]
    ; Re.seq [ Re.char '\xf0'; x90_to_xbf; x80_to_xbf; x80_to_xbf ]
    ; Re.seq [ Re.rg '\xf1' '\xf3'; x90_to_xbf; x80_to_xbf; x80_to_xbf ]
    ; Re.seq [ Re.char '\xf4'; x80_to_x8f; x80_to_xbf; x80_to_xbf ]
    ]
;;

type foldcase_data = (int * int * int) array
type to_re_ctx = { case_sensitive : foldcase_data option }

module Uset = struct
  module M = Map.Make (Uchar)

  let ( >=. ) a b = Uchar.compare a b >= 0
  let ( >. ) a b = Uchar.compare a b > 0
  let ( <=. ) a b = Uchar.compare a b <= 0
  let ( <. ) a b = Uchar.compare a b < 0
  let ( =. ) = Uchar.equal
  let ( <>. ) a b = not (Uchar.equal a b)
  let _ = ( >. ), ( <>. )
  let succ_saturating a = if a =. Uchar.max then a else Uchar.succ a
  let uchar_min a b = if a <. b then a else b
  let uchar_max a b = if a >. b then a else b

  type t = Uchar.t M.t (* maps start of range to inclusive end of range *)

  let empty = M.empty
  let is_empty = M.is_empty
  let singleton c = M.singleton c c
  let range_starting_at_or_after t c = M.find_first_opt (fun c' -> c' >=. c) t
  let range_starting_before t c = M.find_last_opt (fun c' -> c' <. c) t

  (* "interacting" meaning either intersecting, or contiguous such that we'd need to
     merge them *)
  let first_interacting_range t s e =
    match range_starting_before t s with
    | Some (_, e') as rg when s <=. succ_saturating e' -> rg
    | _ ->
      (match range_starting_at_or_after t s with
       | Some (s', _) as rg when s' <=. succ_saturating e -> rg
       | _ -> None)
  ;;

  let rec add_range s e t : t =
    match first_interacting_range t s e with
    | None -> M.add s e t
    | Some (s', e') ->
      if e <. e'
      then M.add (uchar_min s s') e' (M.remove s' t)
      else add_range (uchar_min s s') e (M.remove s' t)
  ;;

  let add_range_incl_excl s e t = if s =. e then t else add_range s (Uchar.pred e) t
  let add_range_excl_incl s e t = if s =. e then t else add_range (Uchar.succ s) e t
  let range s e = M.add s e empty
  let any = range Uchar.min Uchar.max
  let add s t = add_range s s t
  let union ~big:t1 t2 = if is_empty t1 then t2 else M.fold add_range t2 t1

  let first_intersecting_range t s e =
    match range_starting_before t s with
    | Some (_, e') as rg when s <=. e' -> rg
    | _ ->
      (match range_starting_at_or_after t s with
       | Some (s', _) as rg when s' <=. e -> rg
       | _ -> None)
  ;;

  let rec diff_range s e t : t =
    match first_intersecting_range t s e with
    | None -> t
    | Some (s', e') ->
      let max_s_s' = uchar_max s s' in
      if e <= e'
      then M.remove s' t |> add_range_incl_excl s' max_s_s' |> add_range_excl_incl e e'
      else M.remove s' t |> add_range_incl_excl s' max_s_s' |> diff_range s e
  ;;

  let diff t1 t2 = if is_empty t1 then empty else M.fold diff_range t2 t1
  let compl t = diff any t
  let inter ts = compl (List.fold_left (fun acc t -> union ~big:acc (compl t)) empty ts)

  let mem_range s e t =
    (* I think this is supposed to mean: "is [s..e] a subset of t" *)
    match first_intersecting_range t s e with
    | None -> false
    | Some (s', e') -> s' <=. s && e <=. e'
  ;;

  let rec take_same_leading_range rg1 acc l =
    match l with
    | (rg2 :: rem2) :: rem when Stdlib.( = ) rg1 rg2 ->
      take_same_leading_range rg1 (rem2 :: acc) rem
    | _ -> acc, l
  ;;

  let rec share_prefixes acc (l : (char * char) list list) =
    match l with
    | [] -> Re.alt acc
    | [] :: rem ->
      (* no range can be a prefix of an other one, since that would imply a utf8
         encoding would have another utf8 encoding as a prefix *)
      assert (List.for_all List.is_empty rem);
      assert (List.is_empty acc);
      Re.epsilon
    | (rg1 :: rem1) :: rem ->
      let inner_rem, rem = take_same_leading_range rg1 [ rem1 ] rem in
      share_prefixes
        (Re.seq [ Re.rg (fst rg1) (snd rg1); share_prefixes [] inner_rem ] :: acc)
        rem
  ;;

  let to_re t =
    M.fold (fun s e acc -> utf8_rg s e :: acc) t []
    |> List.concat
    |> List.sort (Stdlib.compare : (char * char) list -> _)
    |> share_prefixes []
  ;;

  module S = Set.Make (Uchar)

  let invariant t =
    let last_end = ref None in
    M.iter
      (fun s e ->
        assert (s <=. e);
        (match !last_end with
         | None -> ()
         | Some e' -> assert (Uchar.succ e' <. s));
        last_end := Some e)
      t
  ;;

  let to_set_for_tests t =
    invariant t;
    let rec add_range s e acc =
      let acc = S.add s acc in
      if s =. e then acc else add_range (Uchar.succ s) e acc
    in
    M.fold add_range t S.empty
  ;;
end

module Case_folding = struct
  (* taken from https://github.com/ocaml/ocaml-re/pull/48 *)
  (* Binary search in the array [a].  It is assumed that [a] (which has elements
     of type int * int * int, is sorted so that if i < j, and [a1, b1, _ =
    a.(i)], [a2, b2, _ = a.(j)], then [a1 <= b1 < a2 < b2].  It returns the
     unique triple [a, b, d] as above such that [a <= c <= b] (if one exists),
     or if none exists, the smallest such interval with [c < a], or if none
     exists, raises [Not_found]. *)
  let find_foldcase c a =
    assert (Array.length a > 0);
    let rec loop imin imax =
      let imid = imin + ((imax - imin) / 2) in
      let lo, hi, _ = a.(imid) in
      if c < lo
      then if imid = imin then `Ok a.(imid) else loop imin (imid - 1)
      else if hi < c
      then if imid = imax then `Not_found else loop (imid + 1) imax
      else `Ok a.(imid)
    in
    loop 0 (Array.length a - 1)
  ;;

  (* Closes the characters set [s] under the equivalence relation of unicode
     simple folding. *)
  let case_insens foldcase_table s =
    let s = ref s in
    let rec add c1 c2 =
      if c1 <= c2
      then (
        match find_foldcase c1 foldcase_table with
        | `Ok (a, b, d) ->
          if c1 < a
          then add a c2
          else (
            let cx = min c2 b in
            let c1d = c1 + d in
            let c2d = cx + d in
            if not (Uset.mem_range (Uchar.of_int c1d) (Uchar.of_int c2d) !s)
            then (
              s := Uset.union (Uset.range (Uchar.of_int c1d) (Uchar.of_int c2d)) ~big:!s;
              add c1d c2d);
            add (cx + 1) c2)
        | `Not_found -> ())
    in
    Uset.M.iter (fun c1 c2 -> add (Uchar.to_int c1) (Uchar.to_int c2)) !s;
    !s
  ;;

  let maybe_case_fold ~ctx uset =
    match ctx.case_sensitive with
    | None -> uset
    | Some data -> case_insens data uset
  ;;
end

module Uchar_set = struct
  type t =
    | Class of Re.t Lazy.t
    | Inter of t list
    | Diff of t * t
    | Compl of t list
    | Union of t list
    | Char of Uchar.t
    | Set of Uchar.t list
    | Rg of Uchar.t * Uchar.t

  type res =
    | Gen of Re.t
    | Uset of Uset.t

  let gen = function
    | Gen re -> re
    | Uset uset -> Uset.to_re uset
  ;;

  let rec to_re ~ctx t = gen (to_re_gen ~ctx t)

  and to_re_gen ~ctx = function
    | Class (lazy re) ->
      (match ctx.case_sensitive with
       | None -> Gen re
       | Some _ ->
         failwith "case-insensitive matching of large character classes is not supported")
    | Inter ts ->
      let usets, gens =
        List.fold_left
          (fun (usets, gens) t2 ->
            match to_re_gen ~ctx t2 with
            | Gen a -> usets, a :: gens
            | Uset a -> a :: usets, gens)
          ([], [])
          ts
      in
      if List.is_empty gens then Uset (Uset.inter usets) else invalid_arg "Re_utf8.inter"
    | Diff (t1, t2) ->
      (match to_re_gen ~ctx t1, to_re_gen ~ctx t2 with
       | Uset u1, Uset u2 -> Uset (Uset.diff u1 u2)
       | _ -> invalid_arg "Re_utf8.diff")
    | Compl ts -> to_re_gen ~ctx (Diff (Inter [], Union ts))
    | Union ts ->
      let uset, gens =
        List.fold_left
          (fun (usets, gens) t2 ->
            match to_re_gen ~ctx t2 with
            | Gen a -> usets, a :: gens
            | Uset a -> Uset.union a ~big:usets, gens)
          (Uset.empty, [])
          ts
      in
      if List.is_empty gens then Uset uset else Gen (Re.alt (Uset.to_re uset :: gens))
    | Char uc -> Uset (Case_folding.maybe_case_fold ~ctx (Uset.singleton uc))
    | Set ucs ->
      Uset
        (Case_folding.maybe_case_fold
           ~ctx
           (List.fold_left (fun acc uc -> Uset.add uc acc) Uset.empty ucs))
    | Rg (uc1, uc2) ->
      let uc1, uc2 = if Uchar.compare uc1 uc2 <= 0 then uc1, uc2 else uc2, uc1 in
      Uset (Case_folding.maybe_case_fold ~ctx (Uset.range uc1 uc2))
  ;;

  let to_set_for_tests t =
    match to_re_gen ~ctx:{ case_sensitive = None } t with
    | Gen _ -> invalid_arg "Re_utf8.to_set_for_tests"
    | Uset uset -> Uset.to_set_for_tests uset
  ;;
end

let rev_uchars_of_string ~function_name s =
  let uchars = ref [] in
  let i = ref 0 in
  while !i < String.length s do
    let udecode = String.get_utf_8_uchar s !i in
    i := !i + Uchar.utf_decode_length udecode;
    uchars := Uchar.utf_decode_uchar udecode :: !uchars;
    if not (Uchar.utf_decode_is_valid udecode) then invalid_arg function_name
  done;
  !uchars
;;

type t =
  | Set of Uchar_set.t
  | String of Uchar.t list
  | Wrap0 of Re.t
  | Wrap1 of (Re.t -> Re.t) * t
  | Wrap_many of (Re.t list -> Re.t) * t list
  | Case of
      { sensitive : foldcase_data option
      ; t : t
      }

let to_re t =
  let rec to_re ~ctx = function
    | Set uset -> Uchar_set.to_re ~ctx uset
    | String uchars ->
      Re.seq
        (List.map
           (fun uchar ->
             Uset.to_re (Case_folding.maybe_case_fold ~ctx (Uset.singleton uchar)))
           uchars)
    | Wrap0 re -> re
    | Wrap1 (f, t) -> f (to_re ~ctx t)
    | Wrap_many (f, ts) -> f (List.map (to_re ~ctx) ts)
    | Case { sensitive; t } -> to_re ~ctx:{ case_sensitive = sensitive } t
  in
  to_re ~ctx:{ case_sensitive = None } t
;;

let compile t = Re.compile (to_re t)
let any = Wrap0 any
let str str = String (List.rev (rev_uchars_of_string ~function_name:"Re_utf8.str" str))
let char u = Uchar_set.Char u
let alt l = Wrap_many (Re.alt, l)
let seq l = Wrap_many (Re.seq, l)
let empty = Wrap0 Re.empty
let epsilon = Wrap0 Re.epsilon
let repn t min max = Wrap1 ((fun t -> Re.repn t min max), t)
let rep t = Wrap1 (Re.rep, t)
let rep1 t = Wrap1 (Re.rep1, t)
let opt t = Wrap1 (Re.opt, t)
let bol = Wrap0 Re.bol
let eol = Wrap0 Re.eol
let bos = Wrap0 Re.bos
let eos = Wrap0 Re.eos
let leol = Wrap0 Re.leol
let start = Wrap0 Re.start
let stop = Wrap0 Re.stop
let whole_string t = Wrap1 (Re.whole_string, t)
let longest t = Wrap1 (Re.longest, t)
let shortest t = Wrap1 (Re.shortest, t)
let first t = Wrap1 (Re.first, t)
let greedy t = Wrap1 (Re.greedy, t)
let non_greedy t = Wrap1 (Re.non_greedy, t)
let group ?name t = Wrap1 (Re.group ?name, t)
let no_group t = Wrap1 (Re.no_group, t)
let nest t = Wrap1 (Re.nest, t)

(* can't support mark *)

let case t = Case { sensitive = None; t }
let no_case data t = Case { sensitive = Some data; t }
let cset uset = Set uset
let set s = Uchar_set.Set (rev_uchars_of_string ~function_name:"Re_utf8.set" s)
let inter us = Uchar_set.Inter us
let diff u1 u2 = Uchar_set.Diff (u1, u2)
let compl us = Uchar_set.Compl us
let union us = Uchar_set.Union us
let rg u1 u2 = Uchar_set.Rg (u1, u2)
let class_ re = Uchar_set.Class re
let ( !! ) = Uchar.of_char

(* predefined ascii sets *)

let digit = rg !!'0' !!'9'
let notnl = compl [ char !!'\n' ]
let lower = rg !!'a' !!'z'
let upper = rg !!'A' !!'Z'
let alpha = union [ lower; upper ]
let alnum = union [ alpha; digit ]
let wordc = union [ alnum; char !!'_' ]
let cntrl = union [ rg !!'\000' !!'\031'; char !!'\127' ]
let graph = rg !!'\033' !!'\126'
let print = rg !!'\032' !!'\126'
let ascii = rg !!'\000' !!'\127'
let blank = set "\t "
let xdigit = union [ digit; rg !!'a' !!'f'; rg !!'A' !!'F' ]

let punct =
  union
    [ rg !!'\033' !!'\047'
    ; rg !!'\058' !!'\064'
    ; rg !!'\091' !!'\096'
    ; rg !!'\123' !!'\126'
    ]
;;

let to_set_for_tests = Uchar_set.to_set_for_tests
let create_foldcase_data = Fun.id
