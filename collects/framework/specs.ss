
(module specs mzscheme
  (provide (rename -contract contract)
           ->
           ->d
           ->*
           ->d*
           ;case->
           )
  (require-for-syntax mzscheme
                      (lib "list.ss")
		      (lib "stx.ss" "syntax"))
  
  (require (lib "class.ss"))
  
  (define (raise-error src-info to-blame fmt . args)
    (error 'contract-violation
           (string-append (format "blame: ~a; contract: ~s; " to-blame src-info)
                          (apply format fmt args))))
  
  (define-struct contract (f))
  (define-struct (simple-arrow-contract contract) ())

  (define-syntax -contract
    (lambda (stx)
      (syntax-case stx ()
        [(_ a-contract to-check pos-blame-e neg-blame-e)
         (with-syntax ([src-loc (cond
                                  [(and (syntax-source stx)
                                        (syntax-span stx)
                                        (syntax-line stx)
                                        (syntax-column stx))
                                   (format "~a: ~a.~a[~a]"
                                           (syntax-source stx)
                                           (syntax-line stx)
                                           (syntax-column stx)
                                           (syntax-span stx))]
                                  [(and (syntax-source stx)
                                        (syntax-position stx))
                                   (format "~a: ~a"
                                           (syntax-source stx)
                                           (syntax-position stx))]
                                  [else
                                   (format "~s" (syntax-object->datum (syntax a-contract)))])])
           (syntax
            (-contract a-contract to-check pos-blame-e neg-blame-e src-loc)))]
        [(_ a-contract-e to-check pos-blame-e neg-blame-e src-info-e)
         (syntax
          (let ([a-contract a-contract-e]
                [name to-check]
                [neg-blame neg-blame-e]
                [pos-blame pos-blame-e]
                [src-info src-info-e])
            (unless (contract-p? a-contract)
              (error 'contract "expected a contract as first argument, got: ~e, other args ~e ~e ~e ~e" 
                     a-contract
                     name
                     pos-blame
                     neg-blame
                     src-info))
            (unless (and (symbol? neg-blame)
                         (symbol? pos-blame))
              (error 'contract "expected symbols as names for assigning blame, got: ~e and ~e, other args ~e ~e ~e"
                     neg-blame pos-blame
                     a-contract 
                     name
                     src-info))
            (check-contract a-contract name pos-blame neg-blame src-info-e)))])))
  
  (define-syntaxes (-> ->* ->d ->d*)
    (let ()
      (define (->/f stx)
        (syntax-case stx ()
          [(_) (raise-syntax-error '-> "expected at least one argument" stx)]
          [(_ ct ...)
           (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (ct ...))))]
                         [rng (car (last-pair (syntax->list (syntax (ct ...)))))])
             (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                           [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                           [arity (length (syntax->list (syntax (dom ...))))])
               (syntax
                (let ([dom-x dom] ...
                      [rng-x rng])
                  (unless (contract-p? dom-x)
                    (error '-> "expected contract as argument, got ~e" ct-x)) ...
                  (unless (contract-p? rng-x)
                    (error '-> "expected contract as argument, got: ~e" rng-x))
                  (->* (dom-x ...) (rng-x))))))]))

      (define (->*/f stx)
        (syntax-case stx ()
          [(_ (dom ...) (rng ...))
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                         [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (syntax
              (let ([dom-x dom] ...
                    [rng-x rng] ...)
                (unless (contract-p? dom-x)
                  (error '->* "expected contract as argument, got ~e" ct-x)) ...
                (unless (contract-p? rng-x)
                  (error '->* "expected contract as argument, got: ~e" rng-x)) ...
                (make-simple-arrow-contract
                 (lambda (val pos-blame neg-blame src-info)
                   (if (and (procedure? val)
                            (procedure-arity-includes? val arity))
                       (lambda (arg-x ...)
                         (let-values ([(res-x ...)
                                       (val
                                        (check-contract dom-x arg-x neg-blame pos-blame src-info)
                                        ...)])
                           (values (check-contract
                                    rng-x 
                                    res-x
                                    pos-blame
                                    neg-blame
                                    src-info)
                                   ...)))
                       (raise-error
                        src-info
                        pos-blame
                        "expected a procedure that accepts ~a arguments, got: ~e"
                        arity
                        val)))))))]
          [(_ (dom ...) rest (rng ...))
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                         [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (syntax
              (let ([dom-x dom] ...
                    [dom-rest-x rest]
                    [rng-x rng] ...)
                (unless (contract-p? dom-x)
                  (error '->* "expected contract for domain position, got ~e" dom-x)) ...
                (unless (contract-p? dom-rest-x)
                  (error '->* "expected contract for rest position, got ~e" dom-rest-x))
                (unless (contract-p? rng-x)
                  (error '->* "expected contract for range position, got: ~e" rng-x)) ...
                (make-simple-arrow-contract
                 (lambda (val pos-blame neg-blame src-info)
                   (if (and (procedure? val)
                            (procedure-arity-includes? val arity))
                       (lambda (arg-x ... . rest-arg-x)
                         (let-values ([(res-x ...)
                                       (apply
                                        val
                                        (check-contract dom-x arg-x neg-blame pos-blame src-info)
                                        ...
                                        (check-contract dom-rest-x rest-arg-x neg-blame pos-blame src-info))])
                           (values (check-contract
                                    rng-x 
                                    res-x
                                    pos-blame
                                    neg-blame
                                    src-info)
                                   ...)))
                       (raise-error
                        src-info
                        pos-blame
                        "expected a procedure that accepts ~a arguments, got: ~e"
                        arity
                        val)))))))]))
      
      (define (->d/f stx)
        (syntax-case stx ()
          [(_) (raise-syntax-error '->d "expected at least one argument" stx)]
          [(_ ct ...)
           (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (ct ...))))]
                         [rng (car (last-pair (syntax->list (syntax (ct ...)))))])
             (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                           [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                           [arity (length (syntax->list (syntax (dom ...))))])
               (syntax
                (let ([dom-x dom] ...
                      [rng-x rng])
                  (unless (contract-p? dom-x)
                    (error '->d "expected contract as argument, got ~e" ct-x)) ...
                  (unless (and (procedure? rng-x)
                               (procedure-arity-includes? rng-x arity))
                    (error '->d "expected range portion to be a function that takes ~a arguments, got: ~e"
                           arity
                           rng-x))
                  (make-simple-arrow-contract
                   (lambda (val pos-blame neg-blame src-info)
                     (if (and (procedure? val)
                              (procedure-arity-includes? val arity))
                         (lambda (arg-x ...)
                           (let ([rng-contract (rng-x arg-x ...)])
                             (unless (contract-p? rng-contract)
                               (error '->d "expected range portion to return a contract, got: ~e"
                                      rng-contract))
                             (check-contract 
                              rng-contract
                              (val (check-contract dom-x arg-x neg-blame pos-blame src-info) ...)
                              pos-blame
                              neg-blame
                              src-info)))
                         (raise-error
                          src-info
                          pos-blame
                          "expected a procedure that accepts ~a arguments, got: ~e"
                          arity
                          val))))))))]))
      
      (define (->*d/f stx)
        (syntax-case stx ()
          [(_ (dom ...) rng-mk)
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (syntax
              (let ([dom-x dom] ...
                    [rng-mk-x rng-mk])
                (unless (contract-p? dom-x)
                  (error '->*d "expected contract as argument, got ~e" ct-x)) ...
                (unless (and (procedure? rng-mk-x)
                             (procedure-arity-includes? rng-mk-x arity))
                  (error '->*d "expected range position to be a procedure that accepts ~ arguments, got: ~e"
                         arity rng-mk-x))
                (make-simple-arrow-contract
                 (lambda (val pos-blame neg-blame src-info)
                   (if (and (procedure? val)
                            (procedure-arity-includes? val arity))
                       (lambda (arg-x ...)
                         (call-with-values
                          (lambda ()
                            (rng-mk-x arg-x ...))
                          (lambda rng-contracts
                            (call-with-values
                             (lambda ()
                               (val
                                (check-contract dom-x arg-x neg-blame pos-blame src-info)
                                ...))
                             (lambda results
                               (unless (= (length results) (length rng-contracts))
                                 (error '->d* 
                                        "expected range contract contructor and function to have the same number of values, got ~a and ~a respectively" 
                                        (length results) (length rng-contracts)))
                               (apply 
                                values
                                (map (lambda (rng-contract result)
                                       (check-contract
                                        rng-contract
                                        result
                                        pos-blame
                                        neg-blame
                                        src-info))
                                     range-contracts
                                     results)))))))
                       (raise-error
                        src-info
                        pos-blame
                        "expected a procedure that accepts ~a arguments, got: ~e"
                        arity
                        val)))))))]
          [(_ (dom ...) rest (rng ...))
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                         [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                         [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                         [arity (length (syntax->list (syntax (dom ...))))])
             (syntax
              (let ([dom-x dom] ...
                    [dom-rest-x rest]
                    [rng-x rng] ...)
                (unless (contract-p? dom-x)
                  (error '->* "expected contract for domain position, got ~e" dom-x)) ...
                (unless (contract-p? dom-rest-x)
                  (error '->* "expected contract for rest position, got ~e" dom-rest-x))
                (unless (contract-p? rng-x)
                  (error '->* "expected contract for range position, got: ~e" rng-x)) ...
                (make-simple-arrow-contract
                 (lambda (val pos-blame neg-blame src-info)
                   (if (and (procedure? val)
                            (procedure-arity-includes? val arity))
                       (lambda (arg-x ... . rest-arg-x)
                         (let-values ([(res-x ...)
                                       (apply
                                        val
                                        (check-contract dom-x arg-x neg-blame pos-blame src-info)
                                        ...
                                        (check-contract dom-rest-x rest-arg-x neg-blame pos-blame src-info))])
                           (values (check-contract
                                    rng-x 
                                    res-x
                                    pos-blame
                                    neg-blame
                                    src-info)
                                   ...)))
                       (raise-error
                        src-info
                        pos-blame
                        "expected a procedure that accepts ~a arguments, got: ~e"
                        arity
                        val)))))))]))
      
      (define (all-but-last l)
        (cond
          [(null? l) (error 'all-but-last "bad input")]
          [(null? (cdr l)) null]
          [else (cons (car l) (all-but-last (cdr l)))]))
      
      (values ->/f ->*/f ->d/f ->d*/f)))

  (define (contract-p? val)
    (or (contract? val)
        (and (procedure? val)
             (procedure-arity-includes? val 1))))
  
  (define (check-contract contract val pos neg src-info)
    (cond
      [(contract? contract)
       ((contract-f contract) val pos neg src-info)]
      [else
       (if (contract val)
           val
           (raise-error
            src-info
            pos
            "predicate ~s failed for: ~e"
            contract
            val))]))

#|
  (define-syntax contract/internal
    (lambda (stx)
      (define (all-but-last lst)
        (cond
          [(null? lst) null]
          [(null? (cdr lst)) null]
          [else (cons (car lst) (all-but-last (cdr lst)))]))
      (syntax-case stx ()
	[(_ a-contract name pos-blame neg-blame src-info)
         (and (identifier? (syntax name))
              (identifier? (syntax neg-blame))
              (identifier? (syntax pos-blame)))
         
         (let ()
           ;; build-single-case : syntax[(listof contracts)] -> syntax[(list syntax[args] syntax)]
           ;; builds the arguments and result for a single case of a case-lambda or
           ;; just a single lambda expression.
           (define (build-single-case funs)
             (with-syntax ([(dom ...) (all-but-last (syntax->list funs))]
                           [rng (car (last-pair (syntax->list funs)))])
               (with-syntax ([(ins ...) (generate-temporaries (syntax (dom ...)))])
                 (syntax
                  ((ins ...)
                   (let ([out (name 
                               (contract/internal dom ins neg-blame pos-blame src-info)
                               ...)])
                     (contract/internal rng out pos-blame neg-blame src-info)))))))
           
           (syntax-case (syntax a-contract) (-> ->d ->* case->)
             [(->)
              (raise-syntax-error 
               #f
               "unknown contract specification"
               stx
               (syntax type))]
             [(-> fun funs ...)
              (with-syntax ([(args body) (build-single-case (syntax (fun funs ...)))]
                            [arity (- (length (syntax->list (syntax (fun funs ...))))
                                      1)])
                (syntax
                 (if (and (procedure? name)
                          (procedure-arity-includes? name arity))
                     (lambda args body)
                     (raise-error
                      src-info
                      pos-blame
                      "expected a procedure that accepts ~a arguments, got: ~e"
                      arity
                      name))))]
             [(->* (dom ...) (rngs ...))
              (with-syntax ([arity (length (syntax->list (syntax (dom ...))))]
                            [(dom-vars ...) (generate-temporaries (syntax (dom ...)))]
                            [(rng-vars ...) (generate-temporaries (syntax (rngs ...)))])
                (syntax
                 (if (and (procedure? name)
                          (procedure-arity-includes? name arity))
                     (lambda (dom-vars ...)
                       (let-values ([(rng-vars ...) 
                                     (name
                                      (contract/internal dom dom-vars neg-blame pos-blame src-info)
                                      ...)])
                         (values (contract/internal rngs rng-vars pos-blame neg-blame src-info)
                                 ...)))
                     (raise-error
                      src-info
                      pos-blame
                      "3.expected a procedure that accepts ~a arguments, got: ~e"
                      arity
                      name))))]
             [(case-> (-> funs funss ...) ...)
              (with-syntax ([((args bodies) ...) (map build-single-case
                                                      (syntax->list (syntax ((funs funss ...) ...))))]
                            [(arities ...) (map (lambda (x) (- (length (syntax->list x)) 1))
                                                (syntax->list (syntax ((funs funss ...) ...))))])
                (syntax
                 (if (and (procedure? name)
                          (procedure-arity-includes? name arities) ...)
                     (case-lambda [args bodies] ...)
                     (raise-error
                      src-info
                      pos-blame
                      "1.expected a procedure that accepts these arities: ~a, got: ~e"
                      (list arities ...)
                      name))))]
             [(->d fun funs ...)
              (with-syntax ([(dom ...) (all-but-last (syntax->list (syntax (fun funs ...))))]
                            [rng (car (last-pair (syntax->list (syntax (fun funs ...)))))])
                (with-syntax ([(ins ...) (generate-temporaries (syntax (dom ...)))])
                  (syntax
                   (if (procedure? name)
                       (lambda (ins ...)
                         (let ([->d-rng-contract (rng ins ...)]
                               [out (name (contract/internal dom ins
                                                             neg-blame pos-blame src-info)
                                          ...)])
                           (contract/internal ->d-rng-contract out pos-blame neg-blame src-info)))
                       (raise-error
                        src-info
                        pos-blame
                        "expected a procedure, got: ~e"
                        name)))))]
             [_
              (syntax
               (if (a-contract name)
                   name
                   (raise-error
                    src-info
                    pos-blame
                    "predicate ~s failed for: ~e"
                    'a-contract
                    name)))]))])))
  
|#  
  (provide and/f or/f 
           >=/c <=/c </c >/c 
           false? any? 
           union symbols
           subclass?/c implementation?/c is-a?/c
           listof)

  (define (symbols . ss)
    (lambda (x)
      (memq x ss)))

  (define (>=/c x) (lambda (y) (and (number? y) (>= y x))))
  (define (<=/c x) (lambda (y) (and (number? y) (<= y x))))
  (define (</c x) (lambda (y) (and (number? y) (< y x))))
  (define (>/c x) (lambda (y) (and (number? y) (> y x))))

  (define (is-a?/c <%>) (lambda (x) (is-a? x <%>)))
  (define (subclass?/c <%>) (lambda (x) (subclass? x <%>)))
  (define (implementation?/c <%>) (lambda (x) (implementation? x <%>)))

  (define (false? x) (not x))
  (define (any? x) #t)
  (define (union . fs)
    (for-each
     (lambda (x) 
       (unless (and (procedure? x)
                    (procedure-arity-includes? x 1))
         (error 'union "expected procedures of arity 1, got: ~e" x)))
     fs)
    (apply or/f fs))

  (define (and/f . fs)
    (for-each
     (lambda (x) 
       (unless (and (procedure? x)
                    (procedure-arity-includes? x 1))
         (error 'and/f "expected procedures of arity 1, got: ~e" x)))
     fs)
    (lambda (x)
      (andmap (lambda (f) (f x)) fs)))
  
  (define (or/f . fs)
    (for-each
     (lambda (x) 
       (unless (and (procedure? x)
                    (procedure-arity-includes? x 1))
         (error 'or/f "expected procedures of arity 1, got: ~e" x)))
     fs)
    (lambda (x)
      (ormap (lambda (f) (f x)) fs)))
  
  (define (listof p)
    (lambda (v)
      (and (list? v)
           (andmap p v))))
  )
