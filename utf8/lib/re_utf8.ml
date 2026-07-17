(** This library provides some utf8-aware Re.t combinators. The existing Re combinators
    behave as usual (i.e. matching latin1), and can be used in the same regular
    expression as the combinators we provide here.

    An example of use:

    {[
      let re = Re_utf8.(compile (rep1 (cset Re_utf8.Script.Grek.set))) in
      let matches =
        Re.matches
          re
          {|The word "photography" was created from the Greek roots φωτός (phōtós), genitive of φῶς (phōs), "light"[2] and γραφή (graphé) "representation by means of lines" or "drawing",[3] together meaning "drawing with light".[4]|}
      in
      assert (matches = [ "φωτός"; "φῶς"; "γραφή" ])
    ]}

    Character classes like Script.Grek are the bulk of the functionality provided by
    the library. Because these character classes are large in total and a given program
    is unlikely to need many of them, each one is provided in a separate module so that
    the ocaml compiler can dead-code eliminate unused ones from the final binaries.

    The name for these categories are the same ones from Uucp and utf8 standard. You
    may want to consult https://www.unicode.org/reports/tr44/#General_Category_Values
    to understand what they mean.

    Some things this library doesn't do:
    - no normalization like NFC. In general, the combinators in this library work as
      if the input was a series of uchars, and doesn't deal with the fact that some
      unicode symbols can be expressed in multiple ways.
    - case insensitivity. It could presumably be implemented, but it simply hasn't. *)

include Core
include Aliases
