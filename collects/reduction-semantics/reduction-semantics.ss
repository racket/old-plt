
(module reduction-semantics mzscheme
  (require "private/matcher.ss"
           (lib "contracts.ss")
           (lib "etc.ss"))
  (require-for-syntax (lib "match.ss"))

  (provide reduction
           reduction/context
           language
           replace
	   compiled-lang? red?)
  
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
  (define (build-red lang contractum reduct)
    (make-red (compile-pattern lang contractum) reduct))
  
  (define (compatible-closure red lang nt)
    (context-closure red lang `(cross ,nt)))
  
  (define (context-closure red lang pattern)
    (let ([new-name (gensym 'context-closure)])
      (make-red (compile-pattern
                 lang
                 `(in-hole (name ,new-name ,pattern)
                           ,(red-contractum red)))
                (lambda (bindings)
                  (let ([context (lookup-binding bindings new-name)]
                        [hole (lookup-binding bindings 'hole)]
                        [res ((red-reduct red) bindings)])
                    (replace context hole res))))))
  
  (define-syntax-set (reduction/context reduction)
    
    ;; (reduction/context lang ctxt pattern expression ...)
    (define (reduction/context/proc stx)
      (syntax-case stx ()
        [(_ lang-exp ctxt pattern bodies ...)
         (let ([names (extract-names (syntax-object->datum (syntax pattern)))])
           (with-syntax ([(names ...) (map (lambda (name)
                                             (datum->syntax-object (syntax pattern) name))
                                           names)]
                         [holeg (datum->syntax-object stx (gensym 'hole))]
                         [side-condition-rewritten (rewrite-side-conditions (syntax pattern))])
             (syntax 
              (let ([lang lang-exp])
                (build-red lang
                           `(in-hole* holeg (name context ctxt) side-condition-rewritten)
                           (lambda (bindings)
                             (let ([holeg (lookup-binding bindings 'holeg)]
                                   [context (lookup-binding bindings 'context)]
                                   [names (lookup-binding bindings 'names)] ...)
                               (replace
                                context
                                holeg
                                (begin
                                  (void)
                                  bodies ...)))))))))]))
    
    ;; (reduction lang pattern expression ...)
    (define (reduction/proc stx)
      (syntax-case stx ()
        [(_ lang-exp pattern bodies ...)
         (let ([names (extract-names (syntax-object->datum (syntax pattern)))])
           (with-syntax ([(name ...) (map (lambda (name) (datum->syntax-object (syntax pattern) name)) names)]
                         [hole (datum->syntax-object stx 'hole)]
                         [side-condition-rewritten (rewrite-side-conditions (syntax pattern))])
             (syntax 
              (let ([lang lang-exp])
                (build-red lang
                           `side-condition-rewritten
                           (lambda (bindings)
                             (let ([name (lookup-binding bindings 'name)] ...)
                               bodies ...)))))))]))
    
    (define (rewrite-side-conditions stx)
      (datum->syntax-object
       stx
       (let loop ([term (syntax-object->datum stx)])
         (match term
           [`(side-condition ,pat ,exp)
            (let ([names (extract-names pat)])
              (with-syntax ([exp (datum->syntax-object stx exp)]
                            [pat pat]
                            [(names ...) (map (lambda (x) (datum->syntax-object stx x)) names)])
                (syntax/loc stx
                  (side-condition
                   pat
                   ,(lambda (bindings)
                      (let ([names (lookup-binding bindings 'names)] ...)
                        exp))))))]
           [(? list?) (map loop term)]
           [else term]))))
    
    
    (define (extract-names sexp)
      (let ([dup-names
             (let loop ([sexp sexp]
                        [names null])
               (match sexp
                 [`(name ,(and sym (? symbol?)) ,pat)
                  (loop pat (cons sym names))]
                 [`(in-hole* ,(and sym (? symbol?)) ,pat1 ,pat2)
                  (loop pat1
                        (loop pat2
                              (cons sym names)))]
                 [`(in-hole ,pat1 ,pat2)
                  (loop pat1
                        (loop pat2
                              (cons 'hole names)))]
                 [(? list?)
                  (let i-loop ([sexp sexp]
                               [names names])
                    (cond
                      [(null? sexp) names]
                      [else (i-loop (cdr sexp) (loop (car sexp) names))]))]
                 [else names]))]
            [ht (make-hash-table)])
        (for-each (lambda (name) (hash-table-put! ht name #f)) dup-names)
        (hash-table-map ht (lambda (x y) x)))))  
  (define-syntax (language stx)
    (syntax-case stx ()
      [(_ (name rhs ...) ...)
       (andmap identifier? (syntax->list (syntax/loc stx (name ...))))
       (syntax/loc stx
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
