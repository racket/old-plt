
;; Poor man's stack-trace-on-exceptions/profiler

;; see doc.txt for information

(module errortrace mzscheme

  (define key (gensym 'key))
  (export-indirect key)

  (define (stx-bound-memq ssym l)
    (ormap (lambda (p)
	     (and (syntax? P)
		  (bound-identifier=? ssym p)))
	   l))
  
  (define instrumenting-enabled (make-parameter #t))
  (define error-context-display-depth (make-parameter 10000 (lambda (x) (and (integer? x) x))))
  
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
	  (let ([v (cdr (hash-table-get profile-info key))])
	    (set-car! v (+ (- (current-process-milliseconds) start) (car v))))))))
  
  (define (get-profile-results)
    (hash-table-map profile-info (lambda (key val)
                                   (let ([count (cadr val)]
                                         [time (caddr val)]
                                         [name (cadddr val)]
                                         [cmss (cadddr (cdr val))])
                                     (list count time name
                                           (map
                                            (lambda (cms)
                                              (map (lambda (k)
                                                     (let ([v (cdr (hash-table-get profile-info k))])
                                                       (list (caddr v) (cadddr v))))
                                                   cms))
                                            cmss))))))
  
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
  
  
  (define (profile-point body name expr env)
    (let ([body (map (lambda (e) (annotate e env)) (stx->list body))])
      (if (profiling-enabled)
          (let ([key (gensym)])
            (hash-table-put! profile-info key (list (box #f) 0 0 (or name expr) null))
	    (with-syntax ([key (datum->syntax key #f #f)]
			  [start (datum->syntax (gensym) #f #f)])
	      (with-syntax ([rest 
			     (insert-at-tail*
			      (syntax (register-profile-done 'key start))
			      body)])
		(syntax
		 ((let ([start (register-profile-start 'key)])
		    (with-continuation-mark
		     profile-key
		     'key
		     (begin . rest))))))))
          body)))
  
  (define (insert-at-tail* e exprs)
    (if (stx-null? (cdr exprs))
        (list (insert-at-tail e (stx-car exprs)))
        (cons (stx-car exprs) (insert-at-tail* e (stx-cdr exprs)))))
  
  (define (insert-at-tail se sexpr)
    (with-syntax ([expr sexpr]
		  [e se])
      (syntax-case sexpr (quote #%datum #%unbound
				lambda case-lambda
				let-values letrec-values
				begin begin0 set! struct
				with-continuation-mark
				if #%app)
	;; negligible time to eval
	[id
	 (identifier? sexpr)
	 (syntax (begin e sexpr))]
	[(quote _) (syntax (begin e expr))]
	[(#%datum . d) (syntax (begin e expr))]
	[(#%unbound . d) (syntax (begin e expr))]

	;; No tail effect, and we want to account for the time
	[(lambda . _) (syntax (begin0 expr e))]
	[(case-lambda . _) (syntax (begin0 expr e))]
	[(set! . _) (syntax (begin0 expr e))]
	[(struct . _) (syntax (begin0 expr e))]

	[(let-values bindings . body)
	 (with-syntax ([rest (insert-at-tail* se (syntax body))])
	   (syntax (let-values bindings . rest)))]
	[(letrec-values bindings . body)
	 (with-syntax ([rest (insert-at-tail* se (syntax body))])
	   (syntax (letrec-values bindings . rest)))]

	[(begin . _)
	 (insert-at-tail* se sexpr)]
	[(with-continuation-mark . _)
	 (insert-at-tail* se sexpr)]

	[(begin0 body ...)
	 (syntax (begin0 body ... e))]

	[(if test then)
	 (with-syntax ([then2 (insert-at-tail se (syntax then))])
	   (syntax (if test then2)))]
	[(if test then else)
	 ;; WARNING: e inserted twice!
	 (with-syntax ([then2 (insert-at-tail se (syntax then))]
		       [else2 (insert-at-tail se (syntax else))])
	   (syntax (if test then2 else2)))]

	[(#%app . _)
	 ;; application; exploit guaranteed left-to-right evaluation
	 (insert-at-tail* se sexpr)]
	
	[_else
	 (error 'errortrace
		"unrecognized expression form: ~e"
		(syntax->datum sexpr))])))
  
  (define (annotate-lambda name expr args body env)
    (let ([env (let loop ([v (syntax args)])
		 (cond
		  [(stx-null? v) env]
		  [(identifier? v) (cons v env)]
		  [else (cons (stx-car v) (loop (stx-cdr v)))]))])
      (with-syntax ([body
		     (profile-point 
		      body
		      name expr env)]
		    [args args])
	(syntax (lambda args . body)))))

  (define (annotate-let rec? env varsl rhsl bodyl)
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
			   rhs-env))
			varses 
			rhses)]
		      [(body ...)
		       (map
			(lambda (body)
			  (annotate body env))
			bodies)]
		      [(vars ...) varses]
		      [let (if rec? 
			       (quote-syntax letrec-values)
			       (quote-syntax let-values))])
	  (syntax (let ([vars rhs] ...)
		    body ...))))))

  (define (annotate-seq env who bodyl)
    (with-syntax ([who who]
		  [bodyl
		   (map (lambda (b)
			  (annotate b env))
			(syntax->list bodyl))])
      (syntax (who . bodyl))))
  
  (define (make-annotate top? name)
    (lambda (expr env)
      (syntax-case expr (quote #%datum #%unbound
			       lambda case-lambda
			       let-values letrec-values
			       begin begin0 set! struct
			       with-continuation-mark
			       if #%app
			       define-values define-syntax)
	[_
	 (identifier? expr)
	 (if (stx-bound-memq expr env)
	     ;; lexical variable - no error possile
	     expr
	     ;; might be undefined/uninitialized
	     (with-mark expr expr))]

	[(#%unbound . _)
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
					   env))])
	   (syntax (define-values names marked)))]
	[(begin . exprs)
	 top?
	 (with-syntax ([marked (with-mark expr
					  (annotate
					   (syntax exprs)
					   env))])
	   (syntax (begin . marked)))]
	[(define-syntax name rhs)
	 top?
	 (with-syntax ([marked (with-mark expr
					  (annotate-named
					   (syntax name)
					   (syntax rhs)
					   env))])
	   (syntax (define-syntax name marked)))]
	
	
	[(quote _)
	 expr]

	[(lambda args . body)
	 (annotate-lambda name expr (syntax args) (syntax body) env)]
	[(case-lambda [args . body] ...)
	 (with-syntax ([clauses
			(map
			 (lambda (args body)
			   (annotate-lambda name expr args body env))
			 (syntax->list (syntax (args ...))) 
			 (syntax->list (syntax (body ...))))])
	   (syntax (case-lambda . clauses)))]
	
	[(let-values ([vars rhs] ...) . body)
	 (annotate-let #f env
		       (syntax (vars ...))
		       (syntax (rhs ...))
		       (syntax body))]
	[(letrec-values ([vars rhs] ...) . body)
	 (annotate-let #t env
		       (syntax (vars ...))
		       (syntax (rhs ...))
		       (syntax body))]
	
	[(set! var rhs)
	 (with-syntax ([rhs (annotate-named 
			     (syntax var)
			     (syntax rhs)
			     env)])
	   (syntax (set! var rhs)))]

	[(begin . body)
	 (annotate-seq env (syntax begin) (syntax body))]
	[(begin0 . body)
	 (annotate-seq env (syntax begin0) (syntax body))]
	[(if . body)
	 (annotate-seq env (syntax if) (syntax body))]
	[(#%app . body)
	 (annotate-seq env (syntax #%app) (syntax body))]
	[(with-continuation-mark . body)
	 (annotate-seq env (syntax with-continuation-mark) (syntax body))]

	[(struct (name expr) fields)
	 (with-syntax ([expr (annotate (syntax expr) env)])
	   (syntax (struct (name expr) fields)))]
	[(struct . _)
	 ;; no error possile
	 expr]

	[_else
	 (error 'errortrace
		"unrecognized expression form: ~e"
		(syntax->datum expr))])))
  
  (define annotate (make-annotate #f #f))
  (define annotate-top (make-annotate #t #f))
  (define annotate-named (lambda (name expr env) ((make-annotate #t name) expr env)))
  
  (current-eval
   (let* ([orig (current-eval)]
          [errortrace-eval-handler
           (lambda (e)
	     (let ([a (annotate-top (expand e) null)])
	       (orig a)))])
     errortrace-eval-handler))
  
  (define (cleanup v)
    (cond
     [(and (pair? v)
	   (memq (car v) '(#%datum #%app #%unbound)))
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
                    (cleanup (syntax->datum m))
                    (let ([file (syntax-source m)]
			  [line (syntax-line m)]
			  [col (syntax-line m)])
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
		   ((error-display-handler) (get-output-string p))
		   ((error-escape-handler)))
                 (orig x)))])
     errortrace-exception-handler))
  
  (export print-error-trace 
	  error-context-display-depth 
	  instrumenting-enabled 
	  profiling-enabled
	  profile-paths-enabled 
	  get-profile-results))
 
