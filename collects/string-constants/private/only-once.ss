(module only-once mzscheme
  (provide maybe-print-message)
  
  (define already-printed? #f)
  
  (define (maybe-print-message msg)
    (unless already-printed?
      (set! already-printed? #t)
      (fprintf (current-error-port) "~a" msg))))
