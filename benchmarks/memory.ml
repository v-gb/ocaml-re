open Core
(* This set of benchmarks is designed for testing re's memory usage rather than
   speed. *)

module Bench = Core_bench.Bench

let size = 1_000

(* a pathological re that will consume a bunch of memory *)
let re () =
  let open Re in
  compile @@ seq [ rep (set "01"); char '1'; repn (set "01") size (Some size) ]
;;

(* Another pathological case that is a simplified version of the above *)
let re2 () =
  let open Re in
  seq [ rep (set "01"); char '1'; repn (set "01") size (Some size); char 'x' ] |> compile
;;

let str = "01" ^ String.make size '1'

let stats () =
  let mem_utf8 =
    let s = In_channel.read_all "benchmarks/wikipedia-photographie" in
    let re =
      Re_utf8.(
        compile
          (* rep1 is duplicating the expression, which is kind of expensive with
             unicode. *)
          (rep1 (cset Re_utf8.Gc.L.set)))
    in
    ignore (Re.matches re s : string list);
    [ ("name", "utf8-letter") :: Re__Compile.stats re ]
  in
  let mem_repeat =
    List.concat_map
      [ "re1", re; "re2", re2 ]
      ~f:(fun (name, re) ->
        List.map [ 10; 20; 40; 80; 100; 1000; size ] ~f:(fun len ->
          let re = re () in
          let len = Int.min (String.length str) len in
          ignore (Re.execp ~pos:0 ~len re str);
          ("name", name) :: ("len", Int.to_string len) :: Re__Compile.stats re))
  in
  List.concat [ mem_repeat; mem_utf8 ]
  |> List.map ~f:(sexp_of_list (fun (a, b) -> sexp_of_list sexp_of_string [ a; b ]))
;;

let benchmarks =
  [ "memory 1", re; "memory 2", re2 ]
  |> ListLabels.map ~f:(fun (name, re) ->
    Bench.Test.create_indexed ~name ~args:[ 10; 20; 40; 80; 100; size ] (fun len ->
      Staged.stage (fun () ->
        let re = re () in
        let len = Int.min (String.length str) len in
        ignore (Re.execp ~pos:0 ~len re str))))
;;
