
  (let ([original-outside-world-struct:exn struct:exn])
    (unit/sig mred:exn^
      (import)

      (mred:debug:printf 'invoke "mred:exn@")

      (define-struct (exn original-outside-world-struct:exn) ())
      (define-struct (exn:unknown-preference struct:exn) ())
      (define-struct (exn:during-preferences struct:exn) ())
      (define-struct (exn:url struct:exn) ()) ))
