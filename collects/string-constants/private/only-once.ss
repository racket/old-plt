(module only-once mzscheme
  (provide maybe-print-message)
  
  (define already-printed? #f)
  
  (define (maybe-print-message msg)
    (unless already-printed?
      (set! already-printed? #t)
      ;; the output port may no longer be there, in which case
      ;; we just give up on printing
      (with-handlers ([not-break-exn? (lambda (x) (void))])
        (fprintf (current-error-port) "~a" msg)))))

