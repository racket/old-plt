(let ([original-outside-world-struct:exn struct:exn])
  (unit/sig framework:exn^
    (import)

    (define-struct (exn original-outside-world-struct:exn) ())
    (define-struct (unknown-preference struct:exn) ())
    (define-struct (during-preferences struct:exn) ())
    (define-struct (url struct:exn) ())))
