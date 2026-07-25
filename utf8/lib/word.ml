let not_at_nonspacing_mark =
  (* nonspacing marks should be included in "is_alphabetic", but I suppose
     this lookahead prevents is_alphabetic from cutting the character in half
     to satisfy a later condition *)
  Core.lookahead `Neg (Core.cset Aliases.Gc.Mn.set)
;;

(* This is what I gather from https://www.unicode.org/reports/tr18/, but this is not
   obviously the same thing as the rust regex library, which itself seems to be its
   definition from perl, so maybe we should align with that? *)
let word_char_without_lookahead =
  Core.cset
    (Core.union
       [ Aliases.Alpha.set
       ; Aliases.Gc.Nd.set
       ; Core.char (Uchar.of_int 0x200C)
       ; Core.char (Uchar.of_int 0x200D)
       ])
;;

let wordc = Core.seq [ word_char_without_lookahead; not_at_nonspacing_mark ]

let bow =
  Core.seq
    [ Core.lookbehind `Neg word_char_without_lookahead
    ; Core.lookahead `Pos word_char_without_lookahead
    ; not_at_nonspacing_mark
    ]
;;

let eow =
  Core.seq
    [ Core.lookbehind `Pos word_char_without_lookahead
    ; Core.lookahead `Neg word_char_without_lookahead
    ; not_at_nonspacing_mark
    ]
;;
