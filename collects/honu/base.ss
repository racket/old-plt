(module base mzscheme

  (require (lib "class.ss"))

  (define (println s)
    (display s)
    (newline))

  (provide (all-from mzscheme)
           (all-from (lib "class.ss"))
           println))
