(module read-error-with-stx mzscheme
  
  (require (lib "readerr.ss" "syntax"))
  
  (provide raise-read-error raise-read-error-with-stx)
  ;; Yes, this is misleading for now.  I'll rename it later.
  (define (raise-read-error-with-stx str stx)
    (raise-syntax-error #f str stx))

  )
