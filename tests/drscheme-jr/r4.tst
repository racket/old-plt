(
  ; Quasi-R4RS
  ((cons 3 4))

  ; The docs say this should work in Quasi-R4RS, but it doesn't.
  ; However, if the option, "Unmatched cond/case is an error," is
  ; unchecked in the configure language menu, then this test passes.
  ((void? (cond (#f 32))))

  ; The docs say this should work in Quasi-R4RS, but it doesn't.
  ; However, if the option, "Unmatched cond/case is an error," is
  ; unchecked in the configure language menu, then this test passes.
  ((void? (case 'a ((b c) 32))))

  ((set! some-undefined-identifier 5)
   some-undefined-identifier)

)
