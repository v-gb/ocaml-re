type t = int

let equal (x : int) (y : int) = x = y
let compare (x : int) (y : int) = compare x y
let to_int x = x
let pp = Format.pp_print_int
let intersect x y = x land y <> 0
let ( ++ ) x y = x lor y
let dummy = -1
let inexistant = 1
let ascii_letter = 2
let not_ascii_letter = 4
let newline = 8
let lastnewline = 16
let start_boundary = 32
let stop_boundary = 64
let latin1_letter = 128
let not_latin1_letter = 256
let to_dyn = Dyn.int

let from_char = function
  (* Should match [cword] definition *)
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> ascii_letter ++ latin1_letter
  | '\170' | '\181' | '\186' | '\192' .. '\214' | '\216' .. '\246' | '\248' .. '\255' ->
    not_ascii_letter ++ latin1_letter
  | '\n' -> not_ascii_letter ++ not_latin1_letter ++ newline
  | _ -> not_latin1_letter ++ not_ascii_letter
;;

let letter = function
  | `Ascii -> ascii_letter
  | `Latin1 -> latin1_letter
;;

let not_letter = function
  | `Ascii -> not_ascii_letter
  | `Latin1 -> not_latin1_letter
;;
