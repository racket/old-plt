(define *bad-apps* '())

(define (debug-arity term)
  (cond
    [(zodiac:quote-form? term) #t]
    [(zodiac:lambda-varref? term) #t]
    [(zodiac:top-level-varref/bind/unit? term) #t]
    [(zodiac:case-lambda-form? term)
     (let ([body-l (zodiac:case-lambda-form-bodies term)])
       (andmap (lambda (body)
                 (debug-arity body))
               body-l))]
    [(zodiac:app? term)
     (let* ([fun (zodiac:app-fun term)]
            [args (zodiac:app-args term)]
            [len (length args)]
            [interval (make-Interval len len)]
            [offset (zodiac:location-offset (zodiac:zodiac-start fun))]
            [set-var (make-Set-var (get-var offset))]
            [labels (lookup-hi-and-filter Label? set-var)])
       (printf "offset: ~a~nset-var: ~a~nlabels: ~a~n" offset set-var labels)
       (debug-arity fun)
       (for-each debug-arity args)
       (for-each (lambda (label)
                   (let ([arities (lookup-ars-from-label (Label-name label))])
                     (unless (ormap (lambda (arity)
                                      (satisfies interval arity))
                                    arities)
                       (let ([func-term (lookup-lambda-from-label label)])
                         (printf "label: ~a~n arity: ~a~n" label arities)
                         (set! *bad-apps* (cons term (cons func-term *bad-apps*)))))))
                 labels))]
      [else (error 'unknown "unknown term ~a~n" term)]))
