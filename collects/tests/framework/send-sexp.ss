(define send-sexp
  (lambda (sexp)
    (let-values ([(in out) (tcp-connect "localhost" (require-library "receive-sexps-port.ss" "tests" "framework"))])
      (write sexp out)
      (newline out)
      (read in)
      (close-input-port in)
      (close-output-port out))))
