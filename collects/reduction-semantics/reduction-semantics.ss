
(module reduction-semantics mzscheme
  (require "private/matcher.ss"
           (lib "contracts.ss")
           (lib "etc.ss"))
  (require-for-syntax (lib "list.ss"))

  (provide reduction
           reduction/context
           language
           replace
	   compiled-lang? red?)
  
  (provide/contract
   (language->predicate (compiled-lang? symbol? . -> . (any? . -> . boolean?)))
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
         (let ([names (extract-names (syntax pattern))])
	   (when (null? (syntax->list (syntax (bodies ...))))
	     (raise-syntax-error #f "missing result expression" stx))
           (with-syntax ([(names ...) names]
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
         (let ([names (extract-names (syntax pattern))])
	   (when (null? (syntax->list (syntax (bodies ...))))
	     (raise-syntax-error #f "missing result expression" stx))
           (with-syntax ([(name ...) names]
                         [hole (datum->syntax-object stx 'hole)]
                         [side-condition-rewritten (rewrite-side-conditions (syntax pattern))])
             (syntax 
              (let ([lang lang-exp])
                (build-red lang
                           `side-condition-rewritten
                           (lambda (bindings)
                             (let ([name (lookup-binding bindings 'name)] ...)
                               bodies ...)))))))]))
    
    (define (rewrite-side-conditions orig-stx)
      (let loop ([term orig-stx])
        (syntax-case term (side-condition)
          [(side-condition pat exp)
           (with-syntax ([(names ...) (extract-names (syntax pat))])
             (syntax/loc term
               (side-condition
                pat
                ,(lambda (bindings)
                   (let ([names (lookup-binding bindings 'names)] ...)
                     exp)))))]
          [(terms ...)
           (map loop (syntax->list (syntax (terms ...))))]
          [else term])))
    
    (define (extract-names orig-stx)
      (let ([dups
             (let loop ([stx orig-stx]
                        [names null])
               (syntax-case stx (name in-hole* in-hole)
                 [(name sym pat)
                  (identifier? (syntax sym))
                  (loop (syntax pat) (cons (syntax sym) names))]
                 [(in-hole* sym pat1 pat2)
                  (identifier? (syntax sym))
                  (loop (syntax pat1)
                        (loop (syntax pat2)
                              (cons (syntax sym) names)))]
                 [(in-hole pat1 pat2)
                  (loop (syntax pat1)
                        (loop (syntax pat2)
                              (cons (datum->syntax-object stx 'hole)
                                    names)))]
                 [(pat ...)
                  (let i-loop ([pats (syntax->list (syntax (pat ...)))]
                               [names names])
                    (cond
                      [(null? pats) names]
                      [else (i-loop (cdr pats) (loop (car pats) names))]))]
                 [else names]))])
        (filter-duplicates dups)))
    
    (define (filter-duplicates dups)
      (let loop ([dups dups])
        (cond
          [(null? dups) null]
          [else 
           (cons
            (car dups)
            (filter (lambda (x) (not (module-identifier=? x (car dups))))
                    (loop (cdr dups))))]))))

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
  
  (define (language->predicate lang id)
    (let ([p (compile-pattern lang id)])
      (lambda (x)
	(and (match-pattern p x) #t))))
    
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
