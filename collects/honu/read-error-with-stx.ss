(module read-error-with-stx mzscheme
  
  (require (lib "readerr.ss" "syntax"))
  
  (provide raise-read-error raise-read-error-with-stx)
  (define (raise-read-error-with-stx str stx)
    (raise-read-error str
                      (syntax-source stx)
                      (syntax-line stx)
                      (syntax-column stx)
                      (syntax-position stx)
                      (syntax-span stx)))

  )