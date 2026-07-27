open StdLabels

module Map_plus (Key : Map.OrderedType) = struct
  include Map.Make (Key)

  let of_alist_multi l =
    List.fold_left (List.rev l) ~init:empty ~f:(fun acc (k, v) ->
      let l =
        v
        ::
        (match find_opt k acc with
         | None -> []
         | Some l -> l)
      in
      add k l acc)
  ;;
end

module Char_map = Map_plus (Char)

let string_of_chars chars =
  let b = Bytes.create (List.length chars) in
  List.iteri chars ~f:(fun i elt -> Bytes.set b i elt);
  Bytes.unsafe_to_string b
;;

type trie = A of string * (char * trie) array [@@deriving sexp_of]

let rec triefy l =
  A
    ( List.filter_map l ~f:(function
        | [ s ] -> Some s
        | _ -> None)
      |> List.sort_uniq ~cmp:(fun a b -> Char.compare b a)
      (* Re.set is effectively a bubble sort, and I think this ordering keeps it
         linear instead of quadratic. Not that it matters since we're at compile time I
         guess *)
      |> string_of_chars
    , List.filter_map l ~f:(function
        | [] -> assert false
        | [ _ ] -> None
        | s :: rest -> Some (s, rest))
      |> Char_map.of_alist_multi
      |> Char_map.map triefy
      |> Char_map.to_seq
      |> Array.of_seq )
;;

let ucharset ~bytes_of_uchar uchars =
  let uchars_bytes = List.map uchars ~f:bytes_of_uchar |> triefy in
  let rec to_re (A (set, map)) =
    Re.alt
      (Re.set set
       :: Array.fold_right map ~init:[] ~f:(fun (b, trie) acc ->
         Re.seq [ Re.char b; to_re trie ] :: acc))
  in
  to_re uchars_bytes
;;

let gen1 dst (re : Re.t) =
  let marshalled = Marshal.to_string re [] in
  Out_channel.with_open_bin dst (fun ch ->
    let fmt = Format.formatter_of_out_channel ch in
    Format.fprintf
      fmt
      "let set = Core.class_ (lazy (Marshal.from_string %S 0 : Re.t))@\n"
      marshalled)
;;

let fold_uchars acc f =
  let rec loop acc uc =
    let acc = f acc uc in
    if Uchar.equal uc Uchar.max then acc else loop acc (Uchar.succ uc)
  in
  loop acc Uchar.min
;;

let filter_uchars f = fold_uchars [] (fun acc uc -> if f uc then uc :: acc else acc)

module Gc_map = Map_plus (Uucp.Gc)
module Script_map = Map_plus (Uucp.Script)

let generate ~bytes_of_uchar dst =
  let gc_aliases = ref [] in
  let gc_map =
    fold_uchars (Gc_map.add `Cs [] Gc_map.empty) (fun acc uc ->
      Gc_map.add_to_list (Uucp.Gc.general_category uc) uc acc)
  in
  let gen_gc name l =
    let module_ = "gc_" ^ name in
    gc_aliases := (name, String.capitalize_ascii module_) :: !gc_aliases;
    gen1
      (Filename.concat dst (module_ ^ ".ml"))
      (ucharset
         ~bytes_of_uchar
         (List.concat_map l ~f:(fun uc ->
            try Gc_map.find uc gc_map with
            | Not_found ->
              failwith
                (Format.asprintf
                   "Can't find category %a while building %s"
                   Uucp.Gc.pp
                   uc
                   name))))
  in
  Gc_map.iter (fun gc _ -> gen_gc (Format.asprintf "%a" Uucp.Gc.pp gc) [ gc ]) gc_map;
  (* Following Uucp.Gc, and https://www.unicode.org/reports/tr44/#General_Category_Values
     for the abbreviations *)
  gen_gc "C" [ `Cc; `Cf; `Cn; `Co; `Cs ];
  gen_gc "LC" [ `Lu; `Ll; `Lt ];
  gen_gc "L" [ `Lu; `Ll; `Lt; `Lm; `Lo ];
  gen_gc "M" [ `Mc; `Me; `Mn ];
  gen_gc "N" [ `Nd; `Nl; `No ];
  gen_gc "P" [ `Pc; `Pd; `Pe; `Pf; `Pi; `Po; `Ps ];
  gen_gc "S" [ `Sc; `Sk; `Sm; `So ];
  gen_gc "Z" [ `Zl; `Zp; `Zs ];
  let script_aliases = ref [] in
  let script_map =
    fold_uchars Script_map.empty (fun acc uc ->
      Script_map.add_to_list (Uucp.Script.script uc) uc acc)
  in
  let gen_script s uchars =
    let name = Format.asprintf "%a" Uucp.Script.pp s in
    let module_ = "script_" ^ name in
    script_aliases := (name, String.capitalize_ascii module_) :: !script_aliases;
    gen1 (Filename.concat dst module_ ^ ".ml") (ucharset ~bytes_of_uchar uchars)
  in
  Script_map.iter gen_script script_map;
  let other_aliases = ref [] in
  (let module_ = "alpha" in
   other_aliases := String.capitalize_ascii module_ :: !other_aliases;
   gen1
     (Filename.concat dst module_ ^ ".ml")
     (ucharset ~bytes_of_uchar (filter_uchars Uucp.Alpha.is_alphabetic)));
  (let module_ = "no_case" in
   other_aliases := String.capitalize_ascii module_ :: !other_aliases;
   Out_channel.with_open_bin
     (Filename.concat dst (module_ ^ ".ml"))
     (fun ch ->
       let fmt = Format.formatter_of_out_channel ch in
       No_case.gen fmt));
  (* *)
  Out_channel.with_open_bin (Filename.concat dst "aliases.ml") (fun ch ->
    let fmt = Format.formatter_of_out_channel ch in
    Format.fprintf fmt "@[@[<2>module Gc = struct";
    List.iter (List.rev !gc_aliases) ~f:(fun (name, module_) ->
      Format.fprintf fmt "@\nmodule %s = %s" name module_);
    Format.fprintf fmt "@]@\nend@]@\n";
    Format.fprintf fmt "@[@[<2>module Script = struct";
    List.iter (List.rev !script_aliases) ~f:(fun (name, module_) ->
      Format.fprintf fmt "@\nmodule %s = %s" name module_);
    Format.fprintf fmt "@]@\nend@]@\n";
    List.iter (List.rev !other_aliases) ~f:(fun module_ ->
      Format.fprintf fmt "module %s = %s@\n" module_ module_);
    ())
;;

let utf8_bytes_of_uchar =
  let b = Bytes.create 4 in
  fun uc ->
    let n = Bytes.set_utf_8_uchar b 0 uc in
    let r = ref [] in
    for i = n - 1 downto 0 do
      r := Bytes.unsafe_get b i :: !r
    done;
    !r
;;

let () = generate ~bytes_of_uchar:utf8_bytes_of_uchar "."
