(module b mzscheme
  (require spec/type)

  (define (in-g x) (= x 1))
  (provide/type b in-g g (number -> boolean)))