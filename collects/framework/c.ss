(module c mzscheme
  (require spec/type)
  (require (prefix p: "a.ss") (prefix p: "b.ss"))

  (require/type p:g g (number -> boolean))
  (require/type p:f f ((number -> number) -> number))
  
  (f g))
