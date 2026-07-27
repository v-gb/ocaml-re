(* taken from https://github.com/ocaml/ocaml-re/pull/48 *)

module Cpset = Set.Make (struct
    type t = int

    let compare n m = n - m
  end)

let collect p r =
  Uucd.Cpmap.fold
    (fun cp ps m ->
      match Uucd.find ps p with
      | None -> m
      | Some x -> Uucd.Cpmap.add cp x m)
    r
    Uucd.Cpmap.empty
;;

let ucd inf =
  In_channel.with_open_bin inf (fun ic ->
    let d = Uucd.decoder (`Channel ic) in
    match Uucd.decode d with
    | `Ok db -> db
    | `Error e ->
      let (l0, c0), (l1, c1) = Uucd.decoded_range d in
      failwith (Printf.sprintf "Error: %s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e))
;;

let out ppf name cs =
  Format.fprintf ppf "@\n(* %d range(s) *)@\n" (List.length cs);
  Format.fprintf ppf "@[<hov 2>let %s =@ [|@ " name;
  List.iter (fun (a, b) -> Format.fprintf ppf "(%i, %i);@ " a b) cs;
  Format.fprintf ppf "|]@."
;;

let collect_foldcases r =
  let h = Hashtbl.create 0 in
  let k = ref Cpset.empty in
  Uucd.Cpmap.iter
    (fun cp ps ->
      match Uucd.find ps Uucd.simple_case_folding with
      | None | Some `Self -> ()
      | Some (`Cp cp') ->
        k := Cpset.add cp' !k;
        Hashtbl.add h cp' cp)
    r;
  Cpset.fold (fun cp l -> List.sort compare (cp :: Hashtbl.find_all h cp) :: l) !k []
;;

let make_foldranges ll =
  let l = List.sort (fun (a, _) (a', _) -> a - a') ll in
  let rec loop = function
    | [] -> []
    | (a, b) :: rest ->
      let rec loop1 i = function
        | [] -> []
        | (a', b') :: rest as r ->
          if b' - a' = b - a && a' = a + i + 1
          then loop1 (i + 1) rest
          else (a, a + i, b - a) :: loop r
      in
      loop1 0 rest
  in
  loop l
;;

let out_foldcase ppf r =
  let ll = collect_foldcases r in
  let aux = function
    | [] -> assert false
    | x :: rest ->
      let rec loop a = function
        | [] -> (a, x) :: []
        | b :: rest -> (a, b) :: loop b rest
      in
      loop x rest
  in
  let ll = List.concat (List.map aux ll) in
  let ll = make_foldranges ll in
  Format.fprintf ppf "@\n@[<hov 2>let data = Core.create_foldcase_data [|@\n";
  List.iter (fun (a, b, d) -> Format.fprintf ppf "(%i, %i, %i);@ " a b d) ll;
  Format.fprintf ppf "|]@]@\n";
  Format.fprintf ppf "let re t = Core.no_case data t@\n"
;;

let gen ppf =
  let ucd = ucd "../gen/ucd.all.grouped.xml" in
  out_foldcase ppf ucd.repertoire
;;
