(define mred:exn@
  (let ([original-outside-world-struct:exn struct:exn])
    (unit/s mred:exn^
      (import)
      (define-struct (exn original-outside-world-struct:exn) ())
      (define-struct (exn:unknown-preference struct:exn) ()))))
