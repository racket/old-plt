(define mred:exn@
  (let ([original-outside-world-struct:exn struct:exn])
    (unit/sig mred:exn^
      (import [mred:debug : mred:debug^])

      (mred:debug:printf 'invoke "mred:exn@")

      (define-struct (exn original-outside-world-struct:exn) ())
      (define-struct (exn:unknown-preference struct:exn) ()))))
