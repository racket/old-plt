
(module reduction-semantics mzscheme
  (require "private/matcher.ss"
           (lib "contracts.ss")
           (lib "etc.ss"))
  (require-for-syntax "private/red-sem-macro-helpers.ss")

  (provide reduction
           reduction/context
           language
           replace)
  
  (provide/contract
   (match-pattern (compiled-pattern any? . -> . (union false? (listof bindings?))))
   (compile-pattern (compiled-lang? any? . -> . compiled-pattern))
   (reduce ((listof (lambda (x) (red? x))) any? . -> . (listof any?)))
   (variable-not-in (any? symbol? . -> . symbol?))
   (compatible-closure ((lambda (x) (red? x))
                        compiled-lang?
                        symbol?
                        . -> .
                        (lambda (x) (red? x))))
   (context-closure ((lambda (x) (red? x))
                     compiled-lang?
                     any?
                     . -> .
                     (lambda (x) (red? x)))))
  
  ;; type red = (make-red compiled-pat ((listof (cons sym tst)) -> any)
  (define-struct red (contractum reduct))

  ;; build-red : language pattern ((listof (cons sym tst)) -> any) -> red
  (define build-red
    (opt-lambda (lang contractum reduct [allow-cross? #f])
      (make-red (compile-pattern/cross lang contractum allow-cross?) reduct)))
  
  (define (compatible-closure red lang nt)
    (context-closure red lang `(cross ,nt)))
  
  (define (context-closure red lang pattern)
    (let ([new-name (gensym 'context-closure)])
      (make-red (compile-pattern/cross
                 lang
                 `(in-hole (name ,new-name ,pattern)
                           ,(red-contractum red))
                 #t)
                (lambda (bindings)
                  (let ([context (lookup-binding bindings new-name)]
                        [hole (lookup-binding bindings 'hole)]
                        [res ((red-reduct red) bindings)])
                    (replace context hole res))))))
  
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
