(module specs mzscheme
  (provide contract)
  (require-for-syntax mzscheme
                      (lib "list.ss")
		      (lib "stx.ss" "syntax"))
  
  (require (lib "class.ss"))
  
  (define (raise-error sym fmt . args)
    (apply error 'contract (string-append (format "blame ~a:" sym) fmt) args))
  
  (define-syntax contract
    (lambda (stx)
      (syntax-case stx ()
        [(_ a-contract to-check pos-blame-e neg-blame-e)
         (syntax
          (let ([name to-check]
                [neg-blame neg-blame-e]
                [pos-blame pos-blame-e])
            (unless (and (symbol? neg-blame)
                         (symbol? pos-blame))
              (error 'contract "expected symbols as names for assigning blame, got: ~e and ~e"
                     neg-blame pos-blame))
            (contract/internal stx a-contract name pos-blame neg-blame)))])))
  
  (define-syntax contract/internal
    (lambda (stx)
      (define (all-but-last lst)
        (cond
          [(null? lst) null]
          [(null? (cdr lst)) null]
          [else (cons (car lst) (all-but-last (cdr lst)))]))
      (syntax-case stx ()
	[(_ orig-stx a-contract name pos-blame neg-blame)
         (and (identifier? (syntax name))
              (identifier? (syntax neg-blame))
              (identifier? (syntax pos-blame)))
         
         (syntax-case (syntax a-contract) (-> ->d case-> case->d)
           [(->)
            (raise-syntax-error 
             #f
             "unknown contract specification"
             stx
             (syntax type))]
           [(-> funs ...)
            (with-syntax ([(doms ...) (all-but-last (syntax->list (syntax (funs ...))))]
                          [rng (car (last-pair (syntax->list (syntax (funs ...)))))]
                          [arity (- (length (syntax->list (syntax (funs ...))))
                                    1)])
              (with-syntax ([(ins ...) (generate-temporaries (syntax (doms ...)))])
                (syntax
                 (if (and (procedure? name)
                          (procedure-arity-includes? name arity))
                     (lambda (ins ...)
                       (let ([out (name (contract/internal orig-stx doms ins neg-blame pos-blame) ...)])
                         (contract/internal orig-stx rng out pos-blame neg-blame)))
                     (raise-error
                      pos-blame
                      "expected a procedure that accepts ~a arguments, got: ~e"
                      arity
                      name)))))]
;           [(case-> (cases ...))
;            (let ([cases (syntax->list (syntax cases))])
;              (with-syntax ([bodies (map build-single-body cases)])
;                (syntax
;                 (case-lambda bodies ...))))]
           [(->d funs ...)
            (with-syntax ([(doms ...) (all-but-last (syntax->list (syntax (funs ...))))]
                          [rng (car (last-pair (syntax->list (syntax (funs ...)))))])
              (with-syntax ([(ins ...) (generate-temporaries (syntax (doms ...)))])
                (syntax
                 (if (procedure? name)
                     (lambda (ins ...)
                       (let ([out (name (contract/internal orig-stx doms ins neg-blame pos-blame) ...)])
                         (contract/internal orig-stx (rng ins ...) out pos-blame neg-blame)))
                     (raise-error
                      pos-blame
                      "expected a procedure, got: ~e" name)))))]
           [_
            (syntax
             (if (a-contract name)
                 name
                 (raise-error
                  pos-blame
                  "contract failure: ~e" name)))])])))
  
  
  
  (provide and/f or/f >=/c <=/c </c >/c is-a?/c false?)

  (define (>=/c x) (lambda (y) (>= y x)))
  (define (<=/c x) (lambda (y) (<= y x)))
  (define (</c x) (lambda (y) (< y x)))
  (define (>/c x) (lambda (y) (> y x)))

  (define (is-a?/c <%>) (lambda (x) (is-a? x <%>)))
  (define (false? x) (not x))


  (define (and/f . fs)
    (lambda (x)
      (andmap (lambda (f) (f x)) fs)))
  
  (define (or/f . fs)
    (lambda (x)
      (ormap (lambda (f) (f x)) fs))))
