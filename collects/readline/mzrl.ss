
(module mzrl mzscheme
  
  (define wrote-error? #f)

  (define (readline s)
    (unless wrote-error?
      (set! wrote-error? #t)
      (error 'readline "extension not compiled"))
    (display s)
    (flush-output)
    (read-line))

  (define (add-history l)
    (unless wrote-error?
      (set! wrote-error? #t)
      (error 'add-history "extension not compiled")))

  (provide readline add-history))
