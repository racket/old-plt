
(module stacktrace mzscheme
  (require (lib "unitsig.ss")
           (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax"))
  
  (provide stacktrace@ stacktrace^ stacktrace-imports^)
  
  (define-signature stacktrace-imports^ (with-mark
                                         
                                         test-coverage-enabled
                                         test-covered
                                         initialize-test-coverage-point
                                         
                                         profile-key
                                         profiling-enabled
                                         initialize-profile-point
                                         register-profile-start
                                         register-profile-done))
  (define-signature stacktrace^ (annotate-top 
				 annotate 
				 st-mark-source
				 st-mark-bindings))
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define stacktrace@
    (unit/sig stacktrace^
      (import stacktrace-imports^)
  
      (define-struct st-mark (source))
      (define (st-mark-bindings x) null)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Test case coverage instrumenter

      ;; The next procedure is called by `annotate' and `annotate-top' to wrap
      ;; expressions with test suite coverage information.  Returning the
      ;; first argument means no tests coverage information is collected.
      
      ;; test-coverage-point : syntax syntax -> syntax
      ;; sets a test coverage point for a single expression
      (define (test-coverage-point body expr)
        (if (test-coverage-enabled)
            (let ([key (gensym 'test-coverage-point)])
              (initialize-test-coverage-point key expr)
              (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
                            [body body]
                            [test-covered test-covered])
                (syntax
                 (begin
                   (test-covered 'key)
                   body))))
            body))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Profiling instrumenter

      ;; profile-point : (syntax[list of exprs] symbol-or-#f syntax boolean -> syntax[list of exprs])

      ;; This procedure is called by `annotate' and `annotate-top' to wrap
      ;; expressions with profile collecting information.  Returning the
      ;; first argument means no profiling information is collected.

      ;; The second argument is the point's inferred name, if any, the third
      ;; argument is the source expression, and the fourth argument is #t for
      ;; a transformer expression and #f for a normal expression.

      (define (profile-point bodies name expr trans?)
        (if (profiling-enabled)
            (let ([key (gensym 'profile-point)])
              (initialize-profile-point key name expr)
              (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
                            [start (datum->syntax-object #f (gensym) (quote-syntax here))]
                            [profile-key (datum->syntax-object #f profile-key (quote-syntax here))]
                            [register-profile-start register-profile-start]
                            [register-profile-done register-profile-done])
                (with-syntax ([rest 
                               (insert-at-tail*
                                (syntax (register-profile-done 'key start))
                                bodies
                                trans?)])
                  (syntax
                   ((let ([start (register-profile-start 'key)])
                      (with-continuation-mark 'profile-key 'key
                        (begin . rest))))))))
            bodies))
      
      (define (insert-at-tail* e exprs trans?)
        (if (stx-null? (stx-cdr exprs))
            (list (insert-at-tail e (stx-car exprs) trans?))
            (cons (stx-car exprs) (insert-at-tail* e (stx-cdr exprs) trans?))))
      
      (define (insert-at-tail se sexpr trans?)
        (with-syntax ([expr sexpr]
                      [e se])
          (kernel-syntax-case sexpr trans?
                              ;; negligible time to eval
                              [id
                               (identifier? sexpr)
                               (syntax (begin e expr))]
                              [(quote _) (syntax (begin e expr))]
                              [(quote-syntax _) (syntax (begin e expr))]
                              [(#%datum . d) (syntax (begin e expr))]
                              [(#%top . d) (syntax (begin e expr))]
                              
                              ;; No tail effect, and we want to account for the time
                              [(lambda . _) (syntax (begin0 expr e))]
                              [(case-lambda . _) (syntax (begin0 expr e))]
                              [(set! . _) (syntax (begin0 expr e))]
                              
                              [(let-values bindings . body)
                               (with-syntax ([rest (insert-at-tail* se (syntax body) trans?)])
                                 (syntax (let-values bindings . rest)))]
                              [(letrec-values bindings . body)
                               (with-syntax ([rest (insert-at-tail* se (syntax body) trans?)])
                                 (syntax (letrec-values bindings . rest)))]
                              
                              [(begin . _)
                               (insert-at-tail* se sexpr trans?)]
                              [(with-continuation-mark . _)
                               (insert-at-tail* se sexpr trans?)]
                              
                              [(begin0 body ...)
                               (syntax (begin0 body ... e))]
                              
                              [(if test then)
                               (with-syntax ([then2 (insert-at-tail se (syntax then) trans?)])
                                 (syntax (if test then2)))]
                              [(if test then else)
                               ;; WARNING: e inserted twice!
                               (with-syntax ([then2 (insert-at-tail se (syntax then) trans?)]
                                             [else2 (insert-at-tail se (syntax else) trans?)])
                                 (syntax (if test then2 else2)))]
                              
                              [(#%app . rest)
                               (if (stx-null? (syntax rest))
                                   ;; null constant
                                   (syntax (begin e expr))
                                   ;; application; exploit guaranteed left-to-right evaluation
                                   (insert-at-tail* se sexpr trans?))]
                              
                              [_else
                               (error 'errortrace
                                      "unrecognized (non-top-level) expression form: ~e"
                                      (syntax-object->datum sexpr))])))
      
      ;; Result doesn't have a `lambda', so it works
      ;; for case-lambda
      (define (profile-annotate-lambda name expr args body trans?)
	(with-syntax ([body
		       (profile-point 
                         (map (lambda (e) (annotate e trans?))
                              (stx->list body))
                         name
                         expr
                         trans?)]
		      [args args])
          (syntax (args . body))))
      
      (define (keep-lambda-properties orig new)
        (let ([p (syntax-property orig 'method-arity-error)]
	      [p2 (syntax-property orig 'inferred-name)])
          (let ([new (if p
			 (syntax-property new 'method-arity-error p)
			 new)])
	    (if p2
		(syntax-property new 'inferred-name p2)
		new))))
      
      (define (annotate-let rec? trans? varsl rhsl bodyl)
        (let ([varses (syntax->list varsl)]
              [rhses (syntax->list rhsl)]
              [bodies (syntax->list bodyl)])
	  (with-syntax ([(rhs ...)
			 (map
			  (lambda (vars rhs)
			    (annotate-named
			     (syntax-case vars ()
			       [(id)
				(syntax id)]
			       [_else #f])
			     rhs
			     trans?))
			  varses 
			  rhses)]
			[(body ...)
			 (map
			  (lambda (body)
			    (annotate body trans?))
			  bodies)]
			[(vars ...) varses]
			[let (if rec? 
				 (quote-syntax letrec-values)
				 (quote-syntax let-values))])
	    (syntax (let ([vars rhs] ...)
		      body ...)))))
      
      (define (annotate-seq trans? expr who bodyl annotate)
        (with-syntax ([who who]
                      [bodyl
                       (map (lambda (b)
                              (annotate b trans?))
                            (syntax->list bodyl))])
          (syntax/loc expr (who . bodyl))))
      
      (define (make-annotate top? name)
        (lambda (expr trans?)
          (test-coverage-point 
           (kernel-syntax-case expr trans?
             [_
              (identifier? expr)
              (if (eq? 'lexical (identifier-binding expr))
                  ;; lexical variable - no error possile
                  expr
                  ;; might be undefined/uninitialized
                  (with-mark expr make-st-mark expr))]
             
             [(#%top . _)
              ;; might be undefined/uninitialized
              (with-mark expr make-st-mark expr)]
             [(#%datum . _)
              ;; no error possible
              expr]
             
             ;; Can't put annotation on the outside
             [(define-values names rhs)
              top?
              (with-syntax ([marked (with-mark expr
                                               make-st-mark
                                               (annotate-named
                                                (syntax-case (syntax names) ()
                                                  [(id)
                                                   (syntax id)]
                                                  [_else #f])
                                                (syntax rhs)
                                                trans?))])
                (syntax/loc expr (define-values names marked)))]
             [(begin . exprs)
              top?
              (annotate-seq
               trans? expr (quote-syntax begin)
               (syntax exprs)
               annotate-top)]
             [(define-syntaxes (name ...) rhs)
              top?
              (with-syntax ([marked (with-mark expr
                                               make-st-mark
                                               (annotate-named
                                                (let ([l (syntax->list (syntax (name ...)))])
                                                  (and (pair? l)
                                                       (null? (cdr l))
                                                       (car l)))
                                                (syntax rhs)
                                                #t))])
                (syntax/loc expr (define-syntaxes (name ...) marked)))]
             
             ;; Just wrap body expressions
             [(module name init-import (#%plain-module-begin body ...))
              top?
              (with-syntax ([bodyl
                             (map (lambda (b)
                                    (annotate-top b trans?))
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
              (with-syntax ([cl 
                             (profile-annotate-lambda 
                              name expr 
                              (syntax args) (syntax body) 
                              trans?)])
                (keep-lambda-properties expr (syntax/loc expr (lambda . cl))))]
             [(case-lambda clauses ...)
              (with-syntax ([([args . body] ...)
                             (syntax (clauses ...))])
                (with-syntax ([new-clauses
                               (map
                                (lambda (args body clause) (profile-annotate-lambda name expr args body trans?))
                                (syntax->list (syntax (args ...))) 
                                (syntax->list (syntax (body ...)))
                                (syntax->list (syntax (clauses ...))))])
                  (keep-lambda-properties 
                   expr 
                   (syntax/loc expr 
                     (case-lambda . new-clauses)))))]
             
             ;; Wrap RHSs and body
             [(let-values ([vars rhs] ...) . body)
              (with-mark expr 
                         make-st-mark
                         (annotate-let #f trans?
                                       (syntax (vars ...))
                                       (syntax (rhs ...))
                                       (syntax body)))]
             [(letrec-values ([vars rhs] ...) . body)
              (with-mark expr 
                         make-st-mark
                         (annotate-let #t trans?
                                       (syntax (vars ...))
                                       (syntax (rhs ...))
                                       (syntax body)))]
             
             ;; Wrap RHS
             [(set! var rhs)
              (with-syntax ([rhs (annotate-named 
                                  (syntax var)
                                  (syntax rhs)
                                  trans?)])
                ;; set! might fail on undefined variable, or too many values:
                (with-mark expr make-st-mark (syntax/loc expr (set! var rhs))))]
             
             ;; Wrap subexpressions only
             [(begin . body)
              (with-mark expr
                         make-st-mark
                         (annotate-seq trans? expr (syntax begin) (syntax body) annotate))]
             [(begin0 . body)
              (with-mark expr
                         make-st-mark
                         (annotate-seq trans? expr (syntax begin0) (syntax body) annotate))]
             [(if tst thn els)
              (with-syntax ([w-tst (annotate (syntax tst) trans?)]
                            [w-thn (annotate (syntax thn) trans?)]
                            [w-els (annotate (syntax els) trans?)])
                
                (with-mark
                 expr
                 make-st-mark
                 (syntax/loc expr
                   (if w-tst w-thn w-els))))]
             [(if tst thn)
              (with-syntax ([w-tst (annotate (syntax tst) trans?)]
                            [w-thn (annotate (syntax thn) trans?)])
                (with-mark 
                 expr 
                 make-st-mark 
                 (syntax/loc expr
                   (if w-tst w-thn))))]
             [(with-continuation-mark . body)
              (with-mark expr
                         make-st-mark
                         (annotate-seq 
                          trans? expr (syntax with-continuation-mark) (syntax body) annotate))]
             
             ;; Wrap whole application, plus subexpressions
             [(#%app . body)
              (if (stx-null? (syntax body))
                  ;; It's a null:
                  expr
                  (with-mark expr
                             make-st-mark
                             (annotate-seq trans? expr 
                                           (syntax #%app) (syntax body) 
                                           annotate)))]
             
             [_else
              (error 'errortrace
                     "unrecognized expression form~a: ~e"
                     (if top? " at top-level" "")
                     (syntax-object->datum expr))])
           expr)))
      
      (define annotate (make-annotate #f #f))
      (define annotate-top (make-annotate #t #f))
      (define annotate-named (lambda (name expr trans?) ((make-annotate #t name) expr trans?))))))
