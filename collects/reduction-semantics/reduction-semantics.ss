
(module reduction-semantics mzscheme
  (require "private/matcher.ss"
           (lib "contracts.ss")
           (lib "etc.ss"))
  (require-for-syntax "private/red-sem-macro-helpers.ss")

  (provide reduction 
           reduction/cc
           reduction/context
           language
           replace)
  (provide/contract
   (match-pattern (compiled-pattern any? . -> . (union false? (listof bindings?))))
   (compile-pattern (compiled-lang? any? . -> . compiled-pattern))
   (reduce ((listof (lambda (x) (red? x))) any? . -> . (listof any?)))
   (variable-not-in (any? symbol? . -> . symbol?)))
  
  ;; type red = (make-red compiled-pat ((listof (cons sym tst)) -> any)
  (define-struct red (contractum reduct))

  ;; build-red : language pattern ((listof (cons sym tst)) -> any) -> red
  (define build-red
    (opt-lambda (lang contractum reduct [allow-cross? #f])
      (make-red (compile-pattern/cross lang contractum allow-cross?) reduct)))
  
  ;; (reduction/cc lang nt pattern expression ...)
  (define-syntax (reduction/cc stx)
    (syntax-case stx ()
      [(_ lang-exp nt pattern bodies ...)
       (let* ([names (extract-names (syntax pattern))])
         (unless (identifier? (syntax nt))
           (raise-syntax-error 'reduction/cc "expected name of non-terminal" stx (syntax nt)))
         (with-syntax ([(names ...) (map (lambda (name)
                                           (datum->syntax-object (syntax pattern) name))
                                         names)]
                       [hole (datum->syntax-object stx 'hole)]
                       [context (car (generate-temporaries (list stx)))])
           (syntax/loc stx
            (let ([lang lang-exp])
              (unless (hash-table-get (compiled-lang-ht lang) 'nt (lambda () #f))
                (error 'reduction/cc "unknown non-terminal: ~a" 'nt))
              (build-red lang
                         '(in-hole (name context (cross nt)) pattern)
                         (lambda (bindings)
                           (let ([hole (lookup-binding bindings 'hole)]
                                 [context (lookup-binding bindings 'context)]
                                 [names (lookup-binding bindings 'names)] ...)
                             (replace
                              context
                              hole
                              (begin
                                (void)
                                bodies ...))))
                         #t)))))]))
  
  ;; (reduction/context lang ctxt pattern expression ...)
  (define-syntax (reduction/context stx)
    (syntax-case stx ()
      [(_ lang-exp ctxt pattern bodies ...)
       (let* ([names (extract-names (syntax pattern))])
         (with-syntax ([(names ...) (map (lambda (name)
                                           (datum->syntax-object (syntax pattern) name))
                                         names)]
                       [hole (datum->syntax-object stx 'hole)])
           (syntax 
            (let ([lang lang-exp])
              (build-red lang
                         '(in-hole (name context ctxt) pattern)
                         (lambda (bindings)
                           (let ([hole (lookup-binding bindings 'hole)]
                                 [context (lookup-binding bindings 'context)]
                                 [names (lookup-binding bindings 'names)] ...)
                             (replace
                              context
                              hole
                              (begin
                                (void)
                                bodies ...)))))))))]))
  
  ;; (reduction lang pattern expression ...)
  (define-syntax (reduction stx)
    (syntax-case stx ()
      [(_ lang-exp pattern bodies ...)
       (let* ([names (extract-names (syntax pattern))])
         (with-syntax ([(name ...) (map (lambda (name) (datum->syntax-object (syntax pattern) name)) names)]
                       [hole (datum->syntax-object stx 'hole)])
           (syntax 
            (let ([lang lang-exp])
              (build-red lang
                         'pattern
                         (lambda (bindings)
                           (let ([hole (lookup-binding bindings 'hole (lambda () #f))]
                                 [name (lookup-binding bindings 'name)] ...)
                             bodies ...)))))))]))
  
  (define-syntax (language stx)
    (syntax-case stx ()
      [(_ (name rhs ...) ...)
       (andmap identifier? (syntax->list (syntax (name ...))))
       (syntax
        (compile-language (list (make-nt 'name (list (make-rhs 'rhs) ...)) ...)))]
      [(_ (name rhs ...) ...)
       (for-each
        (lambda (name)
          (unless (identifier? name)
            (raise-syntax-error 'language "expected name" stx name)))
        (syntax->list (syntax (name ...))))]
      [(_ x ...)
       (for-each
        (lambda (x)
          (syntax-case x ()
            [(name rhs ...)
             (void)]
            [_
             (raise-syntax-error 'language "malformed non-terminal" stx x)]))
        (syntax->list (syntax (x ...))))]))
  
    
  ;; reduce : (listof red) exp -> (listof exp)
  (define (reduce reductions exp)
    (let loop ([reductions reductions]
               [acc null])
      (cond
        [(null? reductions) acc]
        [else (let ([red (car reductions)])
                (let ([bindingss (match-pattern (red-contractum red) exp)])
                  (if bindingss
                      (loop (cdr reductions)
                            (map/mt
                             (lambda (bindings) ((red-reduct red) bindings))
                             bindingss
                             acc))
                      (loop (cdr reductions) acc))))])))
  
  ;; map/mt : (a -> b) (listof a) (listof b) -> (listof b)
  ;; map/mt is like map, except it uses the last argument
  ;; instaed of the empty list
  (define (map/mt f l mt-l)
    (let loop ([l l])
      (cond
        [(null? l) mt-l]
        [else (cons (f (car l)) (loop (cdr l)))])))
    
  (define re:gen-d (regexp ".*[^0-9]([0-9]+)$"))
  (define (variable-not-in sexp var)
    (let ([nums (let loop ([sexp sexp]
                           [nums null])
                  (cond
                    [(pair? sexp) (loop (cdr sexp) (loop (car sexp) nums))]
                    [(symbol? sexp) (let ([match (regexp-match re:gen-d (symbol->string sexp))])
                                      (if match
                                          (cons (string->number (cadr match)) nums)
                                          nums))]
                    [else nums]))])
      (if (null? nums)
          (string->symbol (format "~a1" var))
          (string->symbol (format "~a~a" var (+ 1 (apply max nums))))))))
