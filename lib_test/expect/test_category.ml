module Category = Re_private.Category
module Cset = Re_private.Cset

let%expect_test "Category.from_char" =
  for i = 0 to 255 do
    let char = Char.chr i in
    let cat = Category.from_char char in
    assert (
      Bool.equal Cset.(mem (of_char char) cword) Category.(intersect latin1_letter cat));
    assert (
      Bool.equal
        Cset.(mem (of_char char) Ascii.wordc)
        Category.(intersect ascii_letter cat))
  done
;;

let%expect_test "newline" =
  let cat = Category.from_char '\n' in
  assert (Category.(intersect cat newline));
  assert (Category.(intersect cat not_ascii_letter));
  assert (Category.(intersect cat not_latin1_letter))
;;
