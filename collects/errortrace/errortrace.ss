
;; Poor man's stack-trace-on-exceptions/profiler.
;; See doc.txt for information.

(module errortrace mzscheme
  (require (lib "kerncase.ss" "syntax")
	   (lib "stx.ss" "syntax"))
  (require (lib "list.ss") (lib "pretty.ss"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profiling run-time support

  (define profile-thread #f)
  (define profile-key (gensym))
  
  (define profiling-enabled (make-parameter #f))
  (define profile-paths-enabled (make-parameter #f))
  
  (define profile-info (make-hash-table))
  
  (define (register-profile-start key)
    (let ([v (hash-table-get profile-info key)])
      (let ([b (car v)]
	    [v (cdr v)])
	(set-car! v (add1 (car v)))
	(when (profile-paths-enabled)
	  (let ([v (cdddr v)])
	    (set-car! v (cons (current-continuation-marks profile-key) (car v)))))
	(if (unbox b)
	    #f
	    (begin
	      (set-box! b #t)
	      (current-process-milliseconds))))))
  
  (define (register-profile-done key start)
    (when start
      (let ([v (hash-table-get profile-info key)])
	(let ([b (car v)]
	      [v (cddr v)])
	  (set-box! b #f)
	  (let ([v (cddr (hash-table-get profile-info key))])
	    (set-car! v (+ (- (current-process-milliseconds) start) (car v))))))))
  
  (define (get-profile-results)
    (hash-table-map profile-info (lambda (key val)
                                   (let ([count (cadr val)]
                                         [time (caddr val)]
                                         [name (cadddr val)]
                                         [expr (cadddr (cdr val))]
                                         [cmss (cadddr (cddr val))])
                                     (list count time name expr
                                           (map
                                            (lambda (cms)
                                              (map (lambda (k)
                                                     (let ([v (cdr (hash-table-get profile-info k))])
                                                       (list (caddr v) (cadddr v))))
                                                   cms))
                                            cmss))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profiling instrumenter

  (define (profile-point body name expr env trans?)
    (let ([body (map (lambda (e) (annotate e env trans?)) (stx->list body))])
      (if (profiling-enabled)
          (let ([key (gensym)])
            (hash-table-put! profile-info key (list (box #f) 0 0 (and name (syntax-e name)) expr null))
	    (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
			  [start (datum->syntax-object #f (gensym) (quote-syntax here))]
			  [profile-key (datum->syntax-object #f profile-key (quote-syntax here))]
			  [register-profile-start register-profile-start]
			  [register-profile-done register-profile-done])
	      (with-syntax ([rest 
			     (insert-at-tail*
			      (syntax (register-profile-done 'key start))
			      body
			      trans?)])
		(syntax
		 ((let ([start (register-profile-start 'key)])
		    (with-continuation-mark 'profile-key 'key
		      (begin . rest))))))))
          body)))
  
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
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Stacktrace instrumenter

  (define key (gensym 'key))

  ;; with-mark : stx stx -> stx
  (define (with-mark mark expr)
    (with-syntax ([expr expr]
		  [mark mark]
		  [key key])
      (syntax
       (with-continuation-mark
	'key
	(quote-syntax mark)
	expr))))
  
  
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
	   l))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eval handler, exception handler

  (define instrumenting-enabled (make-parameter #t))
  (define error-context-display-depth (make-parameter 10000 (lambda (x) (and (integer? x) x))))
  
  (current-eval
   (let* ([orig (current-eval)]
          [errortrace-eval-handler
           (lambda (e)
	     (let* ([a (if (or (compiled-expression? (if (syntax? e) 
							 (syntax-e e) 
							 e))
			       (not (instrumenting-enabled)))
			   e
			   (annotate-top (expand e) null #f))])
	       (orig a)))])
     errortrace-eval-handler))
  
  (define (cleanup v)
    (cond
     [(and (pair? v)
	   (memq (car v) '(#%datum #%app #%top)))
      (cleanup (cdr v))]
     [(pair? v)
      (cons (cleanup (car v)) (cleanup (cdr v)))]
     [else v]))

  ;; port exn -> void
  ;; effect: prints out the context surrounding the exception
  (define (print-error-trace p x)
    (let loop ([n (error-context-display-depth)]
               [l (continuation-mark-set->list (exn-continuation-marks x) key)])
      (cond
        [(or (zero? n) (null? l)) (void)]
        [(pair? l)
         (let ([m (car l)])
           (fprintf p "  ~e in ~a~n" 
                    (cleanup (syntax-object->datum m))
                    (let ([file (syntax-source m)]
			  [line (syntax-line m)]
			  [col (syntax-column m)])
		      (if (and file line col)
			  (format "~a[~a.~a]"
				  file line col)
                          "UNKNOWN"))))
         (loop (- n 1)
               (cdr l))]
        [else (void)])))
  
  (current-exception-handler
   (let* ([orig (current-exception-handler)]
          [errortrace-exception-handler
           (lambda (x)
             (if (exn? x)
                 (let ([p (open-output-string)])
                   (display (exn-message x) p)
                   (newline p)
                   (print-error-trace p x)
		   ((error-display-handler) (get-output-string p) x)
		   ((error-escape-handler)))
                 (orig x)))])
     errortrace-exception-handler))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Porfile printer

  (define (output-profile-results paths? sort-time?)
    (profiling-enabled #f)
    (error-print-width 50)
    (printf "Sorting profile data...~n")
    (let* ([sel (if sort-time? cadr car)]
	   [counts (quicksort (filter (lambda (c) (positive? (car c))) (get-profile-results))
			      (lambda (a b) (< (sel a) (sel b))))]
	   [total 0])
      (for-each
       (lambda (c)
	 (set! total (+ total (sel c)))
	 (printf "====================================================================~n")
	 (printf "time = ~a : no. = ~a : ~e in ~s~n" (cadr c) (car c) (caddr c) (cadddr c))
	 ;; print call paths
	 (when paths?
	   (for-each
	    (lambda (cms)
	      (unless (null? cms)
		(printf "  VIA ~e" (caar cms))
		(for-each
		 (lambda (cm)
		   (printf " <- ~e" (car cm)))
		 (cdr cms))
		(printf "~n")))
	    (cadddr (cdr c)))))
       counts)
      (printf "Total samples: ~a~n" total)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (provide print-error-trace 
	   error-context-display-depth 
	   instrumenting-enabled 
	   profiling-enabled
	   profile-paths-enabled 
	   get-profile-results
	   output-profile-results))
 
