
(module mzrl mzscheme
  
  (define (readline)
    (error 'readline "extension not compiled"))

  (define (add-history l)
    (error 'add-history "extension not compiled"))

  (provide readline add-history))
