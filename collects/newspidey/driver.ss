(unit/sig newspidey:driver^
  (import [zodiac : zodiac:system^]
          [cft : newspidey:constraints-from-type^]
          [cgp : newspidey:constraints-gen-and-prop^]
          [tr : newspidey:type-reconstruction^]
          [da : newspidey:debug-arity^])

;; ( -> zodiac:read) -> void
(define (newspidey-driver defs-thunk)
    (letrec ([read-from-thunk
              (lambda (thunk)
                (let ([sexp (thunk)])
                  (if (zodiac:eof? sexp)
                      ()
                      (cons sexp (read-from-thunk thunk)))))])
      (parameterize ([current-namespace (make-namespace)])
        (zodiac:prepare-current-namespace-for-vocabulary
         zodiac:scheme-vocabulary)
        (let ([terms (zodiac:scheme-expand-program
                      (read-from-thunk defs-thunk)
                      (zodiac:make-attributes)
                      zodiac:scheme-vocabulary)])
          (cft:init-prim cft:prim-init-list)
          ;;(printf "terms: ~a~n" terms)
          (map (lambda (term) (cgp:derive-top-term-constraints '() term))
               terms)
          (cgp:propagate-constraints)
          (tr:init-set-var-to-type)
          (tr:type-reduce-rec-bindings)
	  (for-each da:debug-arity terms)
          
          ;; debug
          ;;(cgp:pp-constraints cgp:*the-constraints*)
          ))))

  ) ;; unit/sig
