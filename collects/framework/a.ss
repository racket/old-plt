(module a mzscheme
  (require spec/type)

  (define (in-f g) (g 0))
  (provide/type a in-f f ((number -> number) -> number)))

