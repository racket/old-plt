(module util mzscheme
  (require (lib "string.ss"))
  (provide lowercase-symbol!)
  
  ; lowercase-symbol! : (union string bytes) -> symbol
  (define (lowercase-symbol! s)
    (let ([s (if (bytes? s)
                 (bytes->string/utf-8 s)
                 s)])
      (string-lowercase! s)
      (string->symbol s)))
  )