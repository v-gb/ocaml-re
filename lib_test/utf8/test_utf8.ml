let escape =
  (* %S escapes all unicode, which is unreadable *)
  Base.String.Escaping.escape_gen_exn ~escapeworthy_map:[ '"', '"' ] ~escape_char:'\\'
  |> Base.Staged.unstage
;;

let strings a =
  Format.printf
    "[%a]@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt s -> Format.fprintf fmt "\"%s\"" (escape s)))
    a
;;

let text =
  {|The word "photography" was created from the Greek roots φωτός (phōtós), genitive of φῶς (phōs), "light"[2] and γραφή (graphé) "representation by means of lines" or "drawing",[3] together meaning "drawing with light".[4]|}
;;

let%expect_test "general categories" =
  let re = Re_utf8.(compile (rep1 (cset Re_utf8.Gc.L.set))) in
  strings (Re.matches re text);
  [%expect
    {| ["The"; "word"; "photography"; "was"; "created"; "from"; "the"; "Greek"; "roots"; "φωτός"; "phōtós"; "genitive"; "of"; "φῶς"; "phōs"; "light"; "and"; "γραφή"; "graphé"; "representation"; "by"; "means"; "of"; "lines"; "or"; "drawing"; "together"; "meaning"; "drawing"; "with"; "light"] |}]
;;

let%expect_test "script" =
  let re = Re_utf8.(compile (rep1 (cset Re_utf8.Script.Grek.set))) in
  strings (Re.matches re text);
  [%expect {| ["φωτός"; "φῶς"; "γραφή"] |}]
;;

let%expect_test "word" =
  let re = Re_utf8.(compile (rep1 Re_utf8.Word.wordc)) in
  strings (Re.matches re text);
  [%expect
    {| ["The"; "word"; "photography"; "was"; "created"; "from"; "the"; "Greek"; "roots"; "φωτός"; "phōtós"; "genitive"; "of"; "φῶς"; "phōs"; "light"; "2"; "and"; "γραφή"; "graphé"; "representation"; "by"; "means"; "of"; "lines"; "or"; "drawing"; "3"; "together"; "meaning"; "drawing"; "with"; "light"; "4"] |}];
  let re = Re_utf8.(to_re (seq [ Word.bow; cset (set "pφ"); rep Word.wordc ])) in
  strings (Re.matches (Re.compile re) text);
  [%expect {| ["photography"; "φωτός"; "phōtós"; "φῶς"; "phōs"] |}]
;;

let uc str =
  let udecode = String.get_utf_8_uchar str 0 in
  if Uchar.utf_decode_length udecode <> String.length str then invalid_arg "uc";
  Uchar.utf_decode_uchar udecode
;;

let%expect_test "cset union/char/set" =
  let re = Re_utf8.(compile (cset (union [ char (uc "あ"); set "つの" ]))) in
  strings (Re.matches re "あ（以下、12a)の一つで");
  [%expect {| ["あ"; "の"; "つ"] |}]
;;

let%expect_test "cset rg" =
  let re = Re_utf8.(compile (rep1 (cset (rg (uc "å") (uc "ê"))))) in
  let str = "áâãäåæçèéêëìíîïðñò" (* consecutive uchars *) ^ "の一" in
  strings (Re.matches re str);
  [%expect {| ["åæçèéê"] |}];
  (* A range that overlaps the surrogate segment ignores the characters in
     the gap. *)
  let re = Re_utf8.(compile (cset (rg (Uchar.of_int 0xD7FE) (Uchar.of_int 0xE001)))) in
  let str =
    "\u{D7FF}"
    ^ "\237\160\128" (* \u{D800}, if that was accepted *)
    ^ "\237\191\191" (* \u{DFFF}, if that was accepted *)
    ^ "\u{E000}"
  in
  Re.matches re str
  |> List.map (fun s ->
    String.get_utf_8_uchar s 0
    |> Uchar.utf_decode_uchar
    |> Uchar.to_int
    |> Printf.sprintf "#%x")
  |> String.concat " "
  |> print_endline;
  [%expect {| #d7ff #e000 |}]
;;

let greek_alphabet =
  "Α α, Β β, Γ γ, Δ δ, Ε ε, Ζ ζ, Η η, Θ θ, Ι ι, Κ κ, Λ λ, Μ μ, Ν ν, Ξ ξ, Ο ο, Π π, Ρ ρ, \
   Σ σ ς, Τ τ, Υ υ, Φ φ, Χ χ, Ψ ψ, Ω ω"
;;

let%expect_test "cset inter" =
  let re = Re_utf8.(compile (cset (inter [ Script.Grek.set; Gc.Lu.set ]))) in
  print_string (Re.matches re greek_alphabet |> String.concat "");
  [%expect {| ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ |}]
;;

let%expect_test "cset diff" =
  let re = Re_utf8.(compile (cset (diff Script.Grek.set Gc.Lu.set))) in
  print_string (Re.matches re greek_alphabet |> String.concat "");
  [%expect {| αβγδεζηθικλμνξοπρσςτυφχψω |}]
;;

let%expect_test "cset compl" =
  let re = Re_utf8.(compile (rep1 (cset (compl [ Gc.L.set; Gc.Zs.set ])))) in
  strings (Re.matches re text |> List.sort_uniq String.compare);
  [%expect {| ["\""; "\",[3]"; "\".[4]"; "\"[2]"; "("; ")"; "),"] |}]
;;

let%expect_test "uchar set combining" =
  let module S = Set.Make (Uchar) in
  let st = Random.State.make [| 123; -2398478 |] in
  let to_re set =
    let b = Buffer.create 3 in
    S.iter (Buffer.add_utf_8_uchar b) set;
    Re_utf8.set (Buffer.contents b)
  in
  let of_re = Re_utf8.to_set_for_tests in
  let same_set s1 s2 = assert (S.equal s1 s2) in
  let gen_set () =
    let range = 10 in
    let n = Random.State.int st (1 lsl range) in
    let s = ref S.empty in
    for i = 0 to range - 1 do
      if n land (1 lsl i) <> 0 then s := S.add (Uchar.of_char (Char.chr i)) !s
    done;
    !s
  in
  let gen_list ?(min = 0) gen_a max =
    List.init (Random.State.int st (max + 1 - min) + min) (fun _ -> gen_a ())
  in
  let gen_list1 gen_a max = gen_a (), gen_list gen_a max in
  let choose gens =
    let n = Random.State.int st (List.length gens) in
    (List.nth gens n) ()
  in
  let inter () =
    let set1, sets = gen_list1 gen_set 2 in
    same_set
      (List.fold_left S.inter set1 sets)
      (of_re (Re_utf8.inter (List.map to_re (set1 :: sets))))
  in
  let diff () =
    let set1 = gen_set () in
    let set2 = gen_set () in
    same_set (S.diff set1 set2) (of_re (Re_utf8.diff (to_re set1) (to_re set2)))
  in
  (* doesn't seem worth testing compl, considering we'd be restating it in terms of diff,
     which is how it's implemented *)
  let union () =
    let sets = gen_list gen_set 3 in
    same_set
      (List.fold_left S.union S.empty sets)
      (of_re (Re_utf8.union (List.map to_re sets)))
  in
  for _ = 0 to 1000 do
    choose [ inter; diff; union ]
  done
;;

let%expect_test "no_case" =
  let re = Re_utf8.(compile (No_case.re (seq [ rep1 (cset (set "âÉδ")) ]))) in
  let text = "not-that-âéδÂÉΔ" in
  strings (Re.matches re text);
  [%expect {| ["âéδÂÉΔ"] |}];
  let re = Re_utf8.(compile (No_case.re (str "éléphant"))) in
  let text = "ÉLÉPHANT" in
  strings (Re.matches re text);
  [%expect {| ["ÉLÉPHANT"] |}];
  (* No support for matching for non-simple cases like this *)
  let re = Re_utf8.(compile (No_case.re (str "straße"))) in
  let text = "STRASSE" in
  strings (Re.matches re text);
  [%expect {| [] |}]
;;
