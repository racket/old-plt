(module specs mzscheme
  (provide contract)
  (require-for-syntax mzscheme
                      (lib "list.ss")
		      (lib "stx.ss" "syntax"))
  
         
  (define (raise-error sym fmt . args)
    (apply error sym fmt args))

  (define-syntax contract
    (lambda (stx)
      (syntax-case stx ()
        [(_ type to-check pos-blame-e neg-blame-e)
         (syntax
          (let ([name to-check]
                [neg-blame neg-blame-e]
                [pos-blame pos-blame-e])
            (unless (and (symbol? neg-blame)
                         (symbol? pos-blame))
              (error 'contract "expected symbols as names for assigning blame, got: ~e and ~e"
                     neg-blame pos-blame))
            (contract/internal stx type name pos-blame neg-blame)))])))

  (define-syntax contract/internal
    (lambda (stx)
      (define (all-but-last lst)
        (cond
          [(null? lst) null]
          [(null? (cdr lst)) null]
          [else (cons (car lst) (all-but-last (cdr lst)))]))
      (syntax-case stx ()
	[(_ orig-stx type name pos-blame neg-blame)
         (and (identifier? (syntax name))
              (identifier? (syntax neg-blame))
              (identifier? (syntax pos-blame)))
         
         (syntax-case (syntax type) (-> number union boolean interface tst)
           [(-> funs ...)
            (with-syntax ([(doms ...) (all-but-last (syntax->list (syntax (funs ...))))]
                          [rng (car (last-pair (syntax->list (syntax (funs ...)))))])
              (with-syntax ([(ins ...) (generate-temporaries (syntax (doms ...)))])
                (syntax
                 (if (procedure? name)
                     (lambda (ins ...)
                       (let ([out (name (contract/internal orig-stx doms ins neg-blame pos-blame) ...)])
                         (contract/internal orig-stx rng out pos-blame neg-blame)))
                     (raise-error
                      pos-blame
                      "expected a procedure, got: ~e" name)))))]
           [(interface i-e)
            (syntax
             (let ([interface i-e])
               (if (is-a? name interface)
                   name
                   (raise-error
                    pos-blame
                    "expected an instance of ~e, got: ~e" name interface))))]
           [number
            (syntax
             (if (number? name)
                 name
                 (raise-error
                  pos-blame
                  "expected a number, got: ~e" name)))]
           [boolean
            (syntax
             (if (boolean? name)
                 name
                 (raise-error
                  pos-blame
                  "expected a boolean, got: ~e" name)))]
           [tst
            (syntax name)]
           [else
            '(printf "equal: ~s datum equal: ~a~n" 
                    (equal? (syntax ->) (car (syntax-e (syntax type))))
                    (equal? (syntax-object->datum (syntax ->))
                            (syntax-object->datum (car (syntax-e (syntax type))))))
            (raise-syntax-error 
             #f
             "unknown contract specification"
             stx
             (syntax type))])]))))
