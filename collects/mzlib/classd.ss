#|

Rough BNF

(class/d
 super-expresion
 init-args
 ((public var ...)
  (override var ...)
  (inherit var ...)
  (rename (var var) ...))
 
  definitions-and-expressions ...)

;; only thing wrong with above bnf is that the public, etc. clauses
;; can appear multiple times in that same section. 

|#


;; only syntactic checking that should be
;; deferred to resultant class expression is
;; the well-formedness of the init-args.
(require-library "refer.ss")
(define-values/invoke-unit
 (class/d)
 (unit (import)
       (export class/d)
       
       ;; too lazy to import it....
       (define (filter p l)
         (let loop ([l l])
           (cond
             [(null? l) null]
             [else (if (p (car l))
                       (cons (car l) (loop (cdr l)))
                       (loop (cdr l)))])))
       
       (define (validate-clauses clauses)
         (unless (and (list? clauses)
                      (andmap (lambda (x)
                                (and
                                 (list? x)
                                 (> (length x) 0)
                                 (or (eq? (car x) 'public)
                                     (eq? (car x) 'rename)
                                     (eq? (car x) 'inherit)
                                     (eq? (car x) 'override))))
                              clauses))
           (raise-syntax-error 'class/d "illformed clauses" clauses)))
       
       (define (extract-clause keyword well-formed-clause?)
         (lambda (clauses)
           (let loop ([clauses clauses])
             (cond
               [(null? clauses) null]
               [else
                (let ([clause (car clauses)])
                  (if (eq? (car clause) keyword)
                      (begin
                        (unless (well-formed-clause? clause)
                          (raise-syntax-error 'class/d (format "malformed ~a clause: ~s" keyword clause)))
                        (append (cdr clause) (loop (cdr clauses))))
                      (loop (cdr clauses))))]))))
       
       (define extract-public-vars (extract-clause 'public (lambda (x) (andmap symbol? x))))
       (define extract-inherited-vars (extract-clause 'inherit (lambda (x) (andmap symbol? x))))
       (define extract-overriden-vars (extract-clause 'override (lambda (x) (andmap symbol? x))))
       (define extract-renamed-vars (extract-clause 'rename (lambda (x)
                                                              (andmap
                                                               (lambda (x)
                                                                 (and (list? x)
                                                                      (= 2 (length x))
                                                                      (symbol? (car x))
                                                                      (symbol? (cadr x))))
                                                               (cdr x)))))
       
       (define (class/d super init-args clauses . def/exps)
         (validate-clauses clauses)
         (let ([class/d-super (gensym "class/d-super")]
               [public-vars (extract-public-vars clauses)]
               [overriden-vars (extract-overriden-vars clauses)]
               [inherited-vars (extract-inherited-vars clauses)]
               [renamed-vars (extract-renamed-vars clauses)])
           (let-values ([(expanded-def/exps types)
                         (let loop ([def/exps def/exps])
                           (cond
                             [(null? def/exps) (values null null)]
                             [else
                              (let-values ([(expanded-def/exp type) (local-expand-body-expression (car def/exps))]
                                           [(def/exps types) (loop (cdr def/exps))])
                                (values (cons expanded-def/exp def/exps)
                                        (cons type types)))]))])
             (let* ([defined-vars (apply append (map (lambda (def/exp type)
                                                       (if (eq? type '#%define-values)
                                                           (cadr def/exp)
                                                           null))
                                                     expanded-def/exps types))]
                    [private-vars
                     (filter (lambda (x) (not (or (member x public-vars)
                                                  (member x overriden-vars))))
                             defined-vars)])

               (for-each (lambda (pub-var)
                           (unless (member pub-var defined-vars)
                             (raise-syntax-error 'class/d (format "public var ~a not defined" pub-var))))
                         public-vars)
               (for-each (lambda (over-var)
                           (unless (member over-var defined-vars)
                             (raise-syntax-error 'class/d (format "overriden var ~a not defined" over-var))))
                         overriden-vars)
               
               `(let ([,class/d-super ,super])
                  (class ,class/d-super ,init-args
                    
                    (sequence
                      ,@(map (lambda (expanded-def/exp type)
                               (case type
                                 [(#%define-values)
                                  `(set!-values . ,(cdr expanded-def/exp))]
                                 [else expanded-def/exp]))
                             expanded-def/exps types))
                    
                    (rename ,@renamed-vars)
                    (inherit ,@inherited-vars)
                    (private ,@(map (lambda (pv) `(,pv ,pv)) private-vars))
                    (public ,@(map (lambda (pv) `(,pv ,pv)) public-vars))
                    (override ,@(map (lambda (ov) `(,ov ,ov)) overriden-vars))))))))))

(define-macro class/d class/d)

;; Tests

(define (synerr x)
  (with-handlers ([exn:syntax? (lambda (x) (void))])
    (eval x)
    (error 'synerr "not a syntax error: ~s" x)))

(define (test tag ans t)
  (unless (equal? ans t)
    (error 'test "~a: expected ~s got ~s" tag ans t)))

(synerr '(class/d object% ((public x)) (define (x) 1)))
(synerr '(class/d object% () ((public x))))

(test
 1
 1
 (send (make-object (class/d object% () ((public y)) (define (y) 1) (super-init))) y))

(test
 2
 1
 (send (make-object (class/d object% () ((public y)) (define (y) 1) (define (z) 1) (super-init))) y))

(test
 3
 3
 (let ([x 1])
   (make-object 
       (class/d object% () ()
                (set! x 2)
                (set! x 3)
                (super-init)))
   x))

(test
 4
 2
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((override x))
                             (super-init)
                             (define (x) 2)))
       x))


(test
 5
 2
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((inherit x)
                              (public y))
                             (super-init)
                             (define (y) (+ (x) (x)))))
       y))

(test
 6
 2
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((rename [super-x x])
                              (public y))
                             (super-init)
                             (define (y) (+ (super-x) (super-x)))))
       y))

(test
 7
 2
 (send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((rename [super-x x])
                              (override x))
                             (super-init)
                             (define (x) (+ (super-x) (super-x)))))
       x))
