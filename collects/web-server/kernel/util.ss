(module util mzscheme
  (require (lib "string.ss")
           (lib "url.ss" "net")
           (lib "contract.ss")
           )

  (provide/contract
   [lowercase-symbol! ((union string? bytes?) . -> . symbol?)])

  ; lowercase-symbol! : (union string bytes) -> symbol
  (define (lowercase-symbol! s)
    (let ([s (if (bytes? s)
                 (bytes->string/utf-8 s)
                 s)])
      (string-lowercase! s)
      (string->symbol s))))
