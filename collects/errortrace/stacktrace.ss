(module stacktrace mzscheme
  (require (lib "unitsig.ss")
           (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax"))
  
  (provide stacktrace@ stacktrace^ stacktrace-imports^)
  
  (define-signature stacktrace-imports^ (with-mark profile-point))
  (define-signature stacktrace^ (annotate-top annotate))
  
  (define stacktrace@
    (unit/sig stacktrace^
      (import stacktrace-imports^)
      
  ;; Result doesn't have a `lambda', so it works
  ;; for case-lambda
      (define (annotate-lambda name expr args body env trans?)
        (let ([env (let loop ([v (syntax args)])
                     (cond
                       [(stx-null? v) env]
                       [(identifier? v) (cons v env)]
                       [else (cons (stx-car v) (loop (stx-cdr v)))]))])
          (with-syntax ([body
                         (profile-point 
                          body
                          name expr env
                          trans?)]
                        [args args])
            (syntax (args . body)))))
      
      (define (annotate-let rec? env trans? varsl rhsl bodyl)
        (let ([varses (syntax->list varsl)]
              [rhses (syntax->list rhsl)]
              [bodies (syntax->list bodyl)])
          (let* ([body-env 
                  (append (apply append (map syntax->list varses))
                          env)]
                 [rhs-env (if rec? body-env env)])
            (with-syntax ([(rhs ...)
                           (map
                            (lambda (vars rhs)
                              (annotate-named
                               (syntax-case vars ()
                                 [(id)
                                  (syntax id)]
                                 [_else #f])
                               rhs
                               rhs-env
                               trans?))
                            varses 
                            rhses)]
                          [(body ...)
                           (map
                            (lambda (body)
                              (annotate body env trans?))
                            bodies)]
                          [(vars ...) varses]
                          [let (if rec? 
                                   (quote-syntax letrec-values)
                                   (quote-syntax let-values))])
              (syntax (let ([vars rhs] ...)
                        body ...))))))
      
      (define (annotate-seq env trans? expr who bodyl annotate)
        (with-syntax ([who who]
                      [bodyl
                       (map (lambda (b)
                              (annotate b env trans?))
                            (syntax->list bodyl))])
          (syntax/loc expr (who . bodyl))))
      
      (define (make-annotate top? name)
        (lambda (expr env trans?)
          (kernel-syntax-case expr trans?
                              [_
                               (identifier? expr)
                               (if (stx-bound-memq expr env)
	     ;; lexical variable - no error possile
                                   expr
	     ;; might be undefined/uninitialized
                                   (with-mark expr expr))]
                              
                              [(#%top . _)
	 ;; might be undefined/uninitialized
                               (with-mark expr expr)]
                              [(#%datum . _)
	 ;; no error possible
                               expr]
                              
	;; Can't put annotation on the outside
                              [(define-values names rhs)
                               top?
                               (with-syntax ([marked (with-mark expr
                                                                (annotate-named
                                                                 (syntax-case (syntax names) ()
                                                                   [(id)
                                                                    (syntax id)]
                                                                   [_else #f])
                                                                 (syntax rhs)
                                                                 env trans?))])
                                 (syntax/loc expr (define-values names marked)))]
                              [(begin . exprs)
                               top?
                               (annotate-seq
                                env trans? expr (quote-syntax begin)
                                (syntax exprs)
                                annotate-top)]
                              [(define-syntaxes (name ...) rhs)
                               top?
                               (with-syntax ([marked (with-mark expr
                                                                (annotate-named
                                                                 (let ([l (syntax->list (syntax (name ...)))])
                                                                   (and (pair? l)
                                                                        (null? (cdr l))
                                                                        (car l)))
                                                                 (syntax rhs)
                                                                 env #t))])
                                 (syntax/loc expr (define-syntaxes (name ...) marked)))]
                              
	;; Just wrap body expressions
                              [(module name init-import (#%plain-module-begin body ...))
                               top?
                               (with-syntax ([bodyl
                                              (map (lambda (b)
                                                     (annotate-top b env trans?))
                                                   (syntax->list (syntax (body ...))))])
                                 (datum->syntax-object
                                  expr
	    ;; Preserve original #%module-begin:
                                  (list (syntax module) (syntax name) (syntax init-import) 
                                        (cons (syntax #%plain-module-begin) (syntax bodyl)))
                                  expr))]
                              
	;; No way to wrap
                              [(require i ...) expr]
                              [(require-for-syntax i ...) expr]
	;; No error possible (and no way to wrap)
                              [(provide i ...) expr]
                              
	;; No error possible
                              [(quote _)
                               expr]
                              [(quote-syntax _)
                               expr]
                              
	;; Wrap body, also a profile point
                              [(lambda args . body)
                               (with-syntax ([cl (annotate-lambda name expr 
                                                                  (syntax args) (syntax body) 
                                                                  env trans?)])
                                 (syntax/loc expr (lambda . cl)))]
                              [(case-lambda [args . body] ...)
                               (with-syntax ([clauses
                                              (map
                                               (lambda (args body)
                                                 (annotate-lambda name expr args body env trans?))
                                               (syntax->list (syntax (args ...))) 
                                               (syntax->list (syntax (body ...))))])
                                 (syntax/loc expr (case-lambda . clauses)))]
                              
	;; Wrap RHSs and body
                              [(let-values ([vars rhs] ...) . body)
                               (annotate-let #f env trans?
                                             (syntax (vars ...))
                                             (syntax (rhs ...))
                                             (syntax body))]
                              [(letrec-values ([vars rhs] ...) . body)
                               (annotate-let #t env trans?
                                             (syntax (vars ...))
                                             (syntax (rhs ...))
                                             (syntax body))]
                              
	;; Wrap RHS
                              [(set! var rhs)
                               (with-syntax ([rhs (annotate-named 
                                                   (syntax var)
                                                   (syntax rhs)
                                                   env trans?)])
                                 (syntax/loc expr (set! var rhs)))]
                              
	;; Wrap subexpressions only
                              [(begin . body)
                               (annotate-seq env trans? expr (syntax begin) (syntax body) annotate)]
                              [(begin0 . body)
                               (annotate-seq env trans? expr (syntax begin0) (syntax body) annotate)]
                              [(if . body)
                               (annotate-seq env trans? expr (syntax if) (syntax body) annotate)]
                              [(with-continuation-mark . body)
                               (annotate-seq env trans? expr (syntax with-continuation-mark) (syntax body) annotate)]
                              
	;; Wrap whole application, plus subexpressions
                              [(#%app . body)
                               (if (stx-null? (syntax body))
	     ;; It's a null:
                                   expr
                                   (with-mark expr
                                              (annotate-seq env trans? expr 
                                                            (syntax #%app) (syntax body) 
                                                            annotate)))]
                              
                              [_else
                               (error 'errortrace
                                      "unrecognized expression form~a: ~e"
                                      (if top? " at top-level" "")
                                      (syntax-object->datum expr))])))
      
      (define annotate (make-annotate #f #f))
      (define annotate-top (make-annotate #t #f))
      (define annotate-named (lambda (name expr env trans?) ((make-annotate #t name) expr env trans?)))
      
      (define (stx-bound-memq ssym l)
        (ormap (lambda (p)
                 (and (syntax? P)
                      (bound-identifier=? ssym p)))
               l)))))