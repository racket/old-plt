(unit/sig newspidey:debug-arity^
  (import [zodiac : zodiac:system^]
          [setexp : newspidey:datadef-setexp^]
          [cgp : newspidey:constraints-gen-and-prop^]
          [gui : spidey2^])

;; list of application terms with arity errors
(define *bad-apps* '())

;; term -> void
;; checks each app for arity errors and adds erroneous terms
;; to *bad-apps*, which is used in the gui part to flag them in red
(define (debug-arity term)
  (cond
    [(zodiac:quote-form? term) (void)]
    [(zodiac:lambda-varref? term) (void)]
    [(zodiac:top-level-varref/bind/unit? term) (void)]
    [(zodiac:define-values-form? term)
     (debug-arity (zodiac:define-values-form-val term))]
    [(zodiac:case-lambda-form? term)
     (let ([body-l (zodiac:case-lambda-form-bodies term)])
       (for-each (lambda (body)
                   (debug-arity body))
                 body-l))]
    [(zodiac:app? term)
     (let* ([fun (zodiac:app-fun term)]
            [args (zodiac:app-args term)]
            [len (length args)]
            [offset (zodiac:location-offset (zodiac:zodiac-start fun))]
            [set-var (setexp:make-Set-var (gui:get-var offset))] ;; XXX get set-var from term->set-var table and get rid of gui call
            [labels (cgp:lookup-hi-and-filter setexp:Label? set-var)])
       ;;(printf "offset: ~a~nset-var: ~a~nlabels: ~a~n" offset set-var labels)
       (debug-arity fun)
       (for-each debug-arity args)
       (for-each (lambda (label)
                   (let ([arities (cgp:lookup-ars-from-label (setexp:Label-name label))])
                     (unless (ormap (lambda (arity)
                                      (cgp:in-interval len (setexp:Arity-req arity))) ;; has to match at least one interval
                                    arities)
                       (let ([func-term (cgp:lookup-lambda-from-label label)])
                         ;;(printf "label: ~a~n arity: ~a~n" label arities)
                         (set! *bad-apps* (cons term (cons func-term *bad-apps*)))))))
                 labels))]
      [else (error 'debug-arity "unknown term ~a~n" term)]))

  ) ;; unit/sig
