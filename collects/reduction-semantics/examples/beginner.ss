
(module beginner mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
	   (lib "gui.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics")
           (lib "match.ss"))

  #|
  
  `lang' below is actually more generous than beginner, but the
  reductions assume that the programs are all syntactically
  well-formed programs in beginner scheme (with the additional
  constraints that all makers are properly applied and function-
  defined variables are only applied and non-function-defined
  variables are never applied -- except for the maker check,
  these will be in a future version of beginner)
  
  still missing: many primops and characters
  
  are there any primops that take zero arguments?
  (should that be syntacically disallowed?)

  The first test case fails because the beginner spec
  needs to be fixed for that program. Then, the test
  case can be fixed.
  
  |#
  
  (define lang
    (language 
     (p (d/e ...))
     (d/e (define (x x x ...) e)
          (define x (lambda (x x ...) e))
          (define x e)
          (define-struct x (x ...))
          e)
     (e (x e e ...)
        (prim-op e ...) 
        (cond (e e) (e e) ...)
        (cond (e e) ... (else e))
        (if e e e)
        (and e e e ...)
        (or e e e ...)
        empty
        x
        'x
        number
        boolean
        string)
        
     (prim-op + / cons first rest empty?)
     
     (p-ctxt (d/e-v ... d/e-ctxt d/e ...))
     (d/e-ctxt (define x e-ctxt)
               e-ctxt)
     (e-ctxt hole
             (x v ... e-ctxt e ...)
             (prim-op v ... e-ctxt e ...)
             (cond [e-ctxt e] [e e] ...)
             (cond [e-ctxt e] [e e] ... [else e])
             (and e-ctxt e ...)
             (or e-ctxt e ...))
     
     (d/e-v (define x (lambda (x x ...) e))
            (define (x x x ...) e)
            (define-struct x (x ...))
            v)

     (v (maker v ...)
        non-struct-value)
     (non-struct-value number
                       list-value
                       boolean
                       string
                       'x)
     (list-value empty
                 (cons v list-value))
     (boolean true
              false)
     
     (maker (side-condition (name v variable) (maker? v)))
     
     (x (side-condition
         (name 
          x
          (variable-except define
                           define-struct
                           lambda
                           cond
                           else
                           if
                           and
                           or
                           empty
                           true
                           false
                           quote))
         (not (prim-op? x))))))
  
  (define beg-e-subst 
    (subst
     [(? number?)
      (constant)]
     [(? symbol?)
      (variable)]
     ;; slight cheat here -- but since cond, if, and, or, etc
     ;; aren't allowed to be variables (syntactically), we're okay.
     [`(,@(e ...))
      (all-vars '())
      (build (lambda (vars . e) e))
      (subterms '() e)]))
    
  (define (maker? v)
    (and (symbol? v)
         (regexp-match #rx"^make-" (symbol->string v))))
  
  (define p? (language->predicate lang 'p))
  (define prim-op? (language->predicate lang 'prim-op))

  (define-syntax (--> stx)
    (syntax-case stx ()
      [(_ lhs rhs)
       (syntax (reduction/context lang p-ctxt lhs rhs))]))

  (define-syntax (e--> stx)
    (syntax-case stx ()
      [(_ lhs rhs)
       (syntax (reduction lang (in-hole p-ctxt lhs) rhs))]))

  #|
  (define-syntax (template stx)
    (define (build name depth)
      (let loop ([depth depth]
                 [exp name])
        (cond
          [(zero? depth) name]
          [else (loop (- depth 1) 
                      (datum->syntax-object
                       name 
                       `(,exp ...)))])))
    (syntax-case stx ()
      ([_ ([name depth] ...) exp]
       (with-syntax ([(name-d ...) 
                      (map (lambda (name depth)
                             (build name
                                    (syntax-object->datum depth)))
                           (syntax->list (syntax (name ...)))
                           (syntax->list (syntax (depth ...))))])
         (syntax
          (with-syntax ([name name-d] ...)
            (syntax-object->datum (syntax exp))))))))
  
  (let ([xs (list 1 2 3)]
        [ys (list 4 5 6)])
    (template ([xs 1]
               [ys 1])
              ((xs ys) ...)))
  
  |#
  
  (define reductions
    (list
     
     ((and false e ...) . --> . 'false)
     ((and true false e ...) . --> . 'false)
     ((and true true) . --> . 'true)
     ((and true true (name e e) (name es e) ...) . --> . `(and true ,e ,@es))
     ((side-condition (and (name v v) e ...) 
                      (and (not (eq? v 'true)) (not (eq? v 'false))))
      . e--> .
      "and: question result is not true or false")
     ((side-condition (and boolean (name v v) e ...)
                      (and (not (eq? v 'true)) (not (eq? v 'false))))
      . e--> .
      "and: question result is not true or false")
     
     ((or true e ...) . --> . 'true)
     ((or false true e ...) . --> . 'true)
     ((or false false) . --> . 'false)
     ((or false false (name e e) (name es e) ...) . --> . `(or false ,e ,@es))
     ((side-condition (or (name v v) e ...) 
                      (and (not (eq? v 'true)) (not (eq? v 'false))))
      . e--> .
      "or: question result is not true or false")
     ((side-condition (or boolean (name v v) e ...)
                      (and (not (eq? v 'true)) (not (eq? v 'false))))
      . e--> .
      "or: question result is not true or false")
     
     ((if true (name e1 e) (name e2 e)) . --> . e1)
     ((if false (name e1 e) (name e2 e)) . --> . e2)
     ((side-condition (if (name v v) e e)
                      (and (not (eq? v 'false))
                           (not (eq? v 'true))))
      . e--> .
      "if: question result is not true or false")
     
     
     ((cond (false e) (name next (e e)) (name rest (e e)) ...)
      . --> .
      `(cond ,next ,@rest))
     ((cond (false e) (name rest (e e)) ... (name last (else e)))
      . --> . 
      `(cond ,@rest ,last))
     ((cond (true (name e e)) (e e) ...) . --> . e)
     ((cond (true (name e e)) (e e) ... (else e)) . --> . e)
     ((cond (else (name e e))) . --> . e)
     ((side-condition
       (cond ((name v v) e) (e e) ...)
       (and (not (eq? v 'false))
            (not (eq? v 'true))
            (not (eq? v 'else))))
      . e--> .
      "cond: question result is not true or false")
     ((side-condition
       (cond ((name v v) e) (e e) ... (else e))
       (and (not (eq? v 'false))
            (not (eq? v 'true))
            (not (eq? v 'else))))
      . e--> .
      "cond: question result is not true or false")
     ((cond (false e)) . e--> . "cond: all question results were false")

     ((empty? empty) . --> . 'true)
     ((side-condition (empty? (name v v))
                      (not (eq? v 'empty)))
      . --> . 
      'false)
     ((empty?) . e--> . "empty?: expects one argument")
     ((empty? v v v ...) . e--> . "empty?: expects one argument")
 
     ((side-condition (cons v (name v v))
                      (and (not (eq? v 'empty))
                           (not (and (pair? v)
                                     (eq? (car v) 'cons)))))
      . e--> .
      "cons: second argument must be of type <list>")
     
     ((first (cons (name v v) list-value)) . --> . v)
     ((first) . e--> . "first: expects one argument")
     ((first v v v ...) . e--> . "first: expects one argument")
     ((side-condition (first (name v v))
                      (or (not (pair? v))
                          (not (eq? 'cons (car v)))))
      . e--> .
      "first: expects argument of type <pair>")
     
     ((rest (cons v (name list-value list-value))) . --> . list-value)
     ((rest v v v ...) . e--> . "rest: expects one argument")
     ((rest) . e--> . "rest: expects one argument")
     
     ((side-condition (rest (name v v))
                      (or (not (pair? v))
                          (not (eq? 'cons (car v)))))
      . e--> .
      "rest: expects argument of type <pair>")
     
     ((+ (name n number) ...) . --> . (apply + n))
     
     ((side-condition (+ (name arg v) ...)
                      (ormap (lambda (x) (not (number? x))) arg))
      . e--> .
      "+: expects type <number>")
     
     ((side-condition (/ (name n number) (name ns number) ...)
                      (not (ormap zero? ns)))
      . --> .
      (apply / (cons n ns)))
     
     ((side-condition (/ (name n number) (name ns number) ...)
                      (ormap zero? ns))
      . e--> . 
      "/: division by zero")
     
     ((side-condition (/ (name arg v) ...)
                      (ormap (lambda (x) (not (number? x))) arg))
      . e--> .
      "/: expects type <number>")
     
     ;; unbound id application
     (reduction
      lang
      (side-condition
       ((name before d/e-v) ...
        (in-hole (name ctxt d/e-ctxt) ((name f x) (name arg v) ...))
        (name after d/e) ...)
       (and (not (prim-op? f))
            (not (defined? f before))))
      (format "reference to undefined identifier: ~a" f))
     
     ;; procedure application as lambda
     (reduction
      lang
      ((name before d/e-v) ...
       (define (name f x) (lambda ((name var x) ...) (name body e)))
       (name middle d/e-v) ...
       (in-hole (name ctxt d/e-ctxt) ((name f x) (name arg v) ...))
       (name after d/e) ...)
      `(,@before
        (define ,f (lambda (,@var) ,body))
        ,@middle
        ,(replace ctxt hole (multi-subst var arg body))
        ,@after))
     
     ;; define-style procedure application
     (reduction
      lang
      ((name before d/e-v) ...
       (define ((name f x) (name var x) ...) (name body e))
       (name middle d/e-v) ...
       (in-hole (name ctxt d/e-ctxt) ((name f x) (name arg v) ...))
       (name after d/e) ...)
      `(,@before
        (define (,f ,@var) ,body)
        ,@middle
        ,(replace ctxt hole (multi-subst var arg body))
        ,@after))
     
     ;; struct predicate passes
     (reduction
      lang
      (side-condition
       ((name before d/e-v) ...
        (define-struct (name struct x) ((name field x) ...))
        (name middle d/e-v) ...
        (in-hole (name ctxt d/e-ctxt) ((name predicate x) ((name maker maker) v ...)))
        (name after d/e) ...)
       (and (maker-name-match? struct maker)
            (predicate-name-match? struct predicate)))
      `(,@before
        (define-struct ,struct (,@field))
        ,@middle
        ,(replace ctxt hole 'true)
        ,@after))
     
     ;; struct predicate fail to another struct
     (reduction
      lang
      (side-condition
       ((name before d/e-v) ...
        (define-struct (name struct x) ((name field x) ...))
        (name middle d/e-v) ...
        (in-hole (name ctxt d/e-ctxt) ((name predicate x) ((name maker maker) v ...)))
        (name after d/e) ...)
       (and (not (maker-name-match? struct maker))
            (predicate-name-match? struct predicate)))
      `(,@before
        (define-struct ,struct (,@field))
        ,@middle
        ,(replace ctxt hole 'false)
        ,@after))
     
     ;; struct predicate fail to another value
     (reduction
      lang
      (side-condition
       ((name before d/e-v) ...
        (define-struct (name struct x) ((name field x) ...))
        (name middle d/e-v) ...
        (in-hole (name ctxt d/e-ctxt) ((name predicate x) non-struct-value))
        (name after d/e) ...)
       (predicate-name-match? struct predicate))
      `(,@before
        (define-struct ,struct (,@field))
        ,@middle
        ,(replace ctxt hole 'false)
        ,@after))
     
     ;; misapplied selector 1
     (reduction
      lang
      (side-condition
       (d/e-v 
        ...
        (define-struct (name struct x) ((name field x) ...))
        d/e-v ...
        (in-hole d/e-ctxt ((name selector x) ((name maker maker) (name arg v) ...)))
        d/e ...)
       (and (not (maker-name-match? struct maker))
            (selector-name-match? struct field selector)))
      (format "~a: expects argument of matching struct" selector))
     
     ;; misapplied selector 2
     (reduction
      lang
      (side-condition
       (d/e-v 
        ...
        (define-struct (name struct x) ((name field x) ...))
        d/e-v ...
        (in-hole d/e-ctxt ((name selector x) non-struct-value))
        d/e ...)
       (selector-name-match? struct field selector))
      (format "~a: expects argument of matching struct" selector))
     
     ;; well-applied selector
     (reduction
      lang
      (side-condition
       ((name before d/e-v) ...
        (define-struct (name struct x) ((name field x) ...))
        (name middle d/e-v) ...
        (in-hole (name ctxt d/e-ctxt) ((name selector x) ((name maker maker) (name arg v) ...)))
        (name after d/e) ...)
       (and (maker-name-match? struct maker)
            (selector-name-match? struct field selector)))
      `(,@before
        (define-struct ,struct (,@field))
        ,@middle
        ,(replace ctxt hole (list-ref arg (struct-index struct field selector)))
        ,@after))))

  (define (defined? f befores)
    (ormap
     (lambda (before)
       (match before
         [`(define (,a-name ,@(x ...)) ,b)
          (eq? f a-name)]
         [`(define ,a-name (lambda ,@(x ...)))
          (eq? f a-name)]
         [`(define-struct ,struct-name (,@(fields ...)))
          (or (ormap (lambda (field)
                       (eq? f (string->symbol (format "~a-~a" struct-name field))))
                     fields)
              (eq? f (string->symbol (format "make-~a" struct-name)))
              (eq? f (string->symbol (format "~a?" struct-name))))]
         [else #t]))
     befores))
  
  (define (multi-subst vars args body)
    (let loop ([args args]
               [vars vars]
               [body body])
      (cond
        [(and (null? args) (null? vars))
         body]
        [(or (null? args) (null? vars))
         (error 'multi-subst "malformed program")]
        [else (loop (cdr args) 
                    (cdr vars)
                    (beg-e-subst (car vars) (car args) body))])))

  (define (selector-name-match? struct fields selector)
    (ormap (lambda (field) (string=? (format "~a-~a" struct field) 
                                     (symbol->string selector)))
           fields))
  
  (define (struct-index struct init-fields selector)
    (let loop ([i 0]
               [fields init-fields])
      (cond
        [(null? fields) (error 'struct-index "~s ~s ~s" struct init-fields selector)]
        [else (let ([field (car fields)])
                (if (string=? (format "~a-~a" struct field) 
                              (symbol->string selector))
                    i
                    (loop (+ i 1)
                          (cdr fields))))])))
  
  (define (maker-name-match? name maker)
    (let* ([names (symbol->string name)]
           [makers (symbol->string maker)]
           [namel (string-length names)]
           [makerl (string-length makers)])
      (and (makerl . > . namel)
           (string=? (substring makers (- makerl namel) makerl)
                     names))))
  
  (define (predicate-name-match? name predicate)
    (eq? (string->symbol (format "~a?" name)) predicate))

  (define failed-tests 0)
  (define tests 0)
  
  (define (test in out)
    (set! tests (+ tests 1))
    (let/ec k
      (let* ([failed
              (lambda (msg)
                (set! failed-tests (+ failed-tests 1))
                (fprintf (current-error-port) "FAILED: ~a\n" msg)
                (k (void)))]
             [got (normalize in failed)])
        (unless (equal? got out)
          (fprintf (current-error-port) "FAILED:   ~s\ngot:      ~s\nexpected: ~s\n" in got out)
          (set! failed-tests (+ failed-tests 1))))))
  
  (define (normalize term failed)
    (let loop ([term term]
               [n 1000])
      (unless (p? term)
        (failed (format "not a p: ~s" term)))
      (let ([nexts (reduce reductions term)])
        (cond
          [(= n 0) (error 'normalize "found too many reductions")]
          [(null? nexts) term]
          [(string? (car nexts)) (car nexts)]
          [(null? (cdr nexts)) (loop (car nexts) (- n 1))]
          [else (failed (format "found more than one reduction\n ~s\n ->\n~s" term nexts))]))))

  (define (show-test-results)
    (cond
      [(= failed-tests 0) 
       (fprintf (current-error-port) "passed all ~a tests" tests)]
      [else
       (fprintf (current-error-port) "failed ~a out of ~a tests" failed-tests tests)]))
  
  (define (run-tests)
    (set! failed-tests 0)
    (set! tests 0)

    (test
     '((define-struct s ())
       (s? (make-s)))
     '((define-struct s ())
       true))
    
    (test
     '((define-struct s (a b))
       (s-a (make-s 1 3)))
     '((define-struct s (a b))
       1))
    
    (test
     '((define-struct s (a b))
       (s-b (make-s 1 3)))
     '((define-struct s (a b))
       3))
    
    (test
     '((define-struct s (a b))
       (define-struct t (x y))
       (t-x (make-s 1 2)))
     "t-x: expects argument of matching struct")
    
    (test
     '((define-struct t (x y))
       (t-x 12))
     "t-x: expects argument of matching struct")
    
    (test
     '((define-struct s (a b))
       (define-struct t (x y))
       (s? (make-s 1 2)))
     '((define-struct s (a b))
       (define-struct t (x y))
       true))
    
    (test
     '((define-struct s (a b))
       (define-struct t (x y))
       (t? (make-s 1 2)))
     '((define-struct s (a b))
       (define-struct t (x y))
       false))
    
    (test
     '((define (f x) x)
       (f 1))
     '((define (f x) x)
       1))
    
    (test
     '((define (double l) (+ l l))
       (double 2))
     '((define (double l) (+ l l))
       4))
    
    (test
     '((define f (lambda (x) x))
       (f 1))
     '((define f (lambda (x) x))
       1))

    (test
     '((define double (lambda (l) (+ l l)))
       (double 2))
     '((define double (lambda (l) (+ l l)))
       4))
    
    (test
     '((f 1))
     "reference to undefined identifier: f")
    
    (test
     '((f 1)
       (define (f x) x))
     "reference to undefined identifier: f")
    
    (test
     '((make-s 1)
       (define-struct s (a b)))
     "reference to undefined identifier: make-s")
    
    (test
     '((+ 1 2 3))
     '(6))
    
    (test
     '((+ 1 "2" 3))
     "+: expects type <number>")
    
    (test
     '((/ 1 2 3))
     '(1/6))
    
    (test
     '((/ 1 2 0 3))
     "/: division by zero")
    
    (test
     '((/ 1 "2" 3))
     "/: expects type <number>")
    
    (test '((+ 1 (/ (+ 3 5) (+ 2 2)))) '(3))
    
    (test '((cons 1 empty)) '((cons 1 empty)))
    (test '((cons 1 2))
          "cons: second argument must be of type <list>")
    (test '((+ (first (cons 1 2)) 2))
          "cons: second argument must be of type <list>")
    (test '((+ (first (cons 1 empty)) 2))
          '(3))
    
    (test
     '((first (cons 1 empty)))
     '(1))
    
    (test
     '((first 1))
     "first: expects argument of type <pair>")
    
    (test
     '((first 1 2))
     "first: expects one argument")
    
    (test
     '((first))
     "first: expects one argument")
    
    (test
     '((rest (cons 1 empty)))
     '(empty))
    
    (test
     '((rest 1))
     "rest: expects argument of type <pair>")
    
    (test
     '((rest 1 2))
     "rest: expects one argument")
    
    (test
     '((rest))
     "rest: expects one argument")
    
    (test
     '((empty? empty))
     '(true))
    
    (test
     '((empty? 1))
     '(false))
    
    (test
     '((empty?))
     "empty?: expects one argument")
    
    (test
     '((empty? 1 2))
     "empty?: expects one argument")
    
    (test
     '((cond [true 1]))
     '(1))
    
    (test
     '((cond [else 1]))
     '(1))
    
    (test
     '((cond [false 1] [else 2]))
     '(2))
    
    (test
     '((cond [false 1] [false 2]))
     "cond: all question results were false")
    
    (test
     '((cond [1 1]))
     "cond: question result is not true or false")
    
    (test
     '((cond [(empty? empty) 'infinite] [else 3]))
     '('infinite))
    
    (test
     '((and true true 3))
     "and: question result is not true or false")
    
    (test
     '((and 1 true true))
     "and: question result is not true or false")
    
    (test
     '((and true true true false))
     '(false))
    
    (test
     '((and false true))
     '(false))

    (test
     '((or false false 3))
     "or: question result is not true or false")
    
    (test
     '((or 1 false false))
     "or: question result is not true or false")
    
    (test
     '((or false false false true))
     '(true))
    
    (test
     '((or true false))
     '(true))
    
    (test
     '((if 1 2 3))
     "if: question result is not true or false")
    
    (test
     '((if true 'x 'y))
     '('x))
    
    (test
     '((if false 'x 'y))
     '('y))

    (show-test-results))

  (define (run-big-test)
    (test
     '((define-struct pr (hd tl))
       (define (avg l)
         (cond
           [(empty? l) 'infinite]
           [else (/ (sum l) (howmany/acc l 0))]))
       (define (sum l)
         (cond
           [(empty? (pr-tl l)) (pr-hd l)]
           [else (+ (pr-hd l) (sum (pr-tl l)))]))
       (define (howmany/acc l acc)
         (cond
           [(empty? l) acc]
           [else (howmany/acc (pr-tl l) (+ acc 1))]))
       (avg empty)
       (avg (make-pr 3 (make-pr 4 (make-pr 5 empty)))))
     '((define-struct pr (hd tl))
       (define (avg l)
         (cond
           [(empty? l) 'infinite]
           [else (/ (sum l) (howmany/acc l 0))]))
       (define (sum l)
         (cond
           [(empty? (pr-tl l)) (pr-hd l)]
           [else (+ (pr-hd l) (sum (pr-tl l)))]))
       (define (howmany/acc l acc)
         (cond
           [(empty? l) acc]
           [else (howmany/acc (pr-tl l) (+ acc 1))]))
       'infinite
       4))))
