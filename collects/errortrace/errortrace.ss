
;; Poor man's stack-trace-on-exceptions/profiler

;; see doc.txt for information

(invoke-open-unit
 (unit 
   (import)
   (export error-context-display-depth instrumenting-enabled profiling-enabled profile-paths-enabled get-profile-results)
   
   (define key (gensym 'key))

   (define instrumenting-enabled (make-parameter #t))
   (define error-context-display-depth (make-parameter 10000 (lambda (x) (and (integer? x) x))))

   (define profile-thread #f)
   (define profile-key (gensym))

   (define profiling-enabled (make-parameter #f))
   (define profile-paths-enabled (make-parameter #f))

   (define profile-info (make-hash-table))

   (define (register-profile-start key b)
     (let ([v (hash-table-get profile-info key)])
       (set-car! v (add1 (car v)))
       (when (profile-paths-enabled)
	 (let ([v (cdddr v)])
	   (set-car! v (cons (current-continuation-marks profile-key) (car v))))))
     (if (unbox b)
	 #f
	 (begin
	   (set-box! b #t)
	   (current-process-milliseconds))))
   
   (define (register-profile-done key b start)
     (when start
       (set-box! b #f)
       (let ([v (cdr (hash-table-get profile-info key))])
	 (set-car! v (+ (- (current-process-milliseconds) start) (car v))))))

   (define (get-profile-results)
     (hash-table-map profile-info (lambda (key val)
				    (let ([count (car val)]
					  [time (cadr val)]
					  [name (caddr val)]
					  [file (cadddr val)]
					  [cmss (cadddr (cdr val))])
				      (list count time name file
					    (map
					     (lambda (cms)
					       (map (lambda (k)
						      (let ([v (hash-table-get profile-info k)])
							(list (caddr v) (cadddr v))))
						    cms))
					     cmss))))))

   (define (with-mark mark expr)
     `(#%with-continuation-mark
       (#%quote ,key) (#%quote (,current-file . ,mark))
       ,expr))

   (define (profile-point body name expr env)
     (let ([body (map (lambda (e) (annotate e env)) body)])
       (if (profiling-enabled)
	   (let ([key (gensym)]
		 [start (gensym)]
		 [box (box #f)])
	     (hash-table-put! profile-info key (list 0 0 (or name expr) current-file null))
	     `((#%let ([,start (,register-profile-start (#%quote ,key) ,box)])
		      (#%with-continuation-mark
		       (#%quote ,profile-key) (#%quote ,key)
		       (#%begin
			,@(insert-at-tail*
			   `(,register-profile-done (#%quote ,key) ,box ,start)
			   body))))))
	   body)))
   
   (define (insert-at-tail* e exprs)
     (if (null? (cdr exprs))
	 (list (insert-at-tail e (car exprs)))
	 (cons (car exprs) (insert-at-tail* e (cdr exprs)))))

   (define (insert-at-tail e expr)
     (cond
      [(not (pair? expr))
       `(#%begin ,e ,expr)]
      [else (case (car expr)
	      [(#%quote)
	       ;; expr doesn't take a significant amount of time to evaluate
	       `(#%begin ,e ,expr)]
	      [(#%lambda #%case-lambda
			 #%unit #%compound-unit
			 #%set!
			 #%struct #%class*/names #%interface)
	       ;; No tail effect, and we want to account for the time
	       `(#%begin0 ,expr ,e)]
	      [(#%let-values #%letrec-values)
	       `(,(car expr)
		 ,(cadr expr)
		 ,@(insert-at-tail* e (cddr expr)))]
	      [(#%begin #%with-continuation-mark)
	       (insert-at-tail* e expr)]
	      [(#%begin0)
	       `(,@expr ,e)]
	      [(#%if)
	       ;; WARNING: e inserted twice!
	       `(#%if ,(cadr expr)
		      ,(insert-at-tail e (caddr expr))
		      ,@(if (null? (cdddr expr))
			    null
			    (list (insert-at-tail e (cadddr expr)))))]
	      [(#%cond) (#%cond)] ; it's an error, so nevermind
	      [(#%invoke-unit #%invoke-open-unit)
	       (let ([u (gensym)])
		 `(#%let ([,u ,(cadr expr)])
		    ,e
		    (,(car expr) ,u ,@(cddr expr))))]
	      [else
	       ;; application; exploit guaranteed left-to-right evaluation
	       (insert-at-tail* e expr)])]))

   (define (make-annotate top? name)
     (lambda (expr env)
       (cond
	[(symbol? expr)
	 (if (memq expr env)
	     ;; lexical variable - no error possile
	     expr
	     ;; could be top-level variable
	     (with-mark expr expr))]
	[(and top?
	      (pair? expr)
	      (memq (car expr)
		    '(#%define-values #%define-macro
				      #%define-id-macro
				      #%define-expansion-time
				      #%begin)))
	 ;; Can't put annotation on the outside
	 (if (eq? (car expr) '#%begin)
	     `(#%begin ,@(map (lambda (e) (annotate-top e env)) (cdr expr)))
	     `(,(car expr) ,(cadr expr) 
			   ,(with-mark expr (annotate-named
					     (and (eq? (car expr) '#%define-values)
						  (= 1 (length (cadr expr)))
						  (caadr expr))
					     (caddr expr)
					     env))))]
	[(and (pair? expr) (eq? (car expr) '#%quote))
	 expr]
	[(pair? expr)
	 (with-mark
	  expr
	  (case (car expr)
	    [(#%quote) expr]
	    [(#%lambda)
	     `(#%lambda ,(cadr expr)
			,@(profile-point 
			   (cddr expr)
			   name expr env))]
	    [(#%case-lambda)
	     `(#%case-lambda 
	       ,@(map (lambda (clause)
			`(,(car clause) 
			  ,@(profile-point
			     (cdr clause)
			     name expr env)))
		      (cdr expr)))]
	    [(#%let-values #%letrec-values)
	     (let* ([vars (apply append (map car (cadr expr)))]
		    [body-env (append vars env)]
		    [rhs-env (if (eq? (car expr) '#%letrec-values)
				 body-env
				 env)])
	       `(,(car expr)
		 ,(map (lambda (clause)
			 `(,(car clause) ,(annotate-named
					   (and (= (length (car clause)) 1)
						(caar clause))
					   (cadr clause)
					   rhs-env)))
		       (cadr expr))
		 ,@(map (lambda (e) (annotate e body-env)) (cddr expr))))]
	    [(#%set!)
	     `(#%set! ,(cadr expr) ,(annotate-named (cadr expr) (caddr expr) env))]
	    [(#%begin #%begin0 #%if #%with-continuation-mark)
	     `(,(car expr) ,@(map (lambda (e) (annotate e env)) (cdr expr)))]
	    [(#%cond) expr]
	    [(#%struct)
	     (if (pair? (cadr expr))
		 `(#%struct (,(caadr expr) ,(annotate (cadadr expr) env)) ,@(cddr expr))
		 expr)]
	    [(#%unit)
	     (let* ([vars (let loop ([body (cdddr expr)])
			    (cond
			     [(null? body) null]
			     [(and (pair? (car body))
				   (eq? '#%define-values (caar body)))
			      (append (cadar body) (loop (cdr body)))]
			     [else (loop (cdr body))]))]
		    [body-env (append vars env)])
	       `(#%unit ,(cadr expr) ,(caddr expr)
			,@(map (lambda (e) (annotate-top e body-env)) (cdddr expr))))]
	    [(#%compound-unit)
	     `(#%compound-unit
	       ,(cadr expr)
	       (link ,@(map (lambda (clause)
			      `(,(car clause) (,(annotate (caadr clause) env) ,@(cdadr clause))))
			    (cdaddr expr)))
	       ,(cadddr expr))]
	    [(#%invoke-unit #%invoke-open-unit)
	     `(,(car expr) ,(annotate (cadr expr) env) ,@(cddr expr))]
	    [(#%class*/names)
	     (let* ([car* (lambda (v)
			    (if (pair? v)
				(car v)
				v))]
		    [init-vars (let loop ([l (list-ref expr 4)])
				 (cond
				  [(pair? l) (cons (car* (car l))
						   (loop (cdr l)))]
				  [(symbol? l) (list l)]
				  [else null]))]
		    [ivars (apply
			    append
			    (map (lambda (clause)
				   (case (car clause)
				     [(sequence) null]
				     [(inherit rename) (map car* (cdr clause))]
				     [(public override private)
				      (map car* (map car* (cdr clause)))]))
				 (list-tail expr 5)))]
		    [body-env (append init-vars ivars)])
	       `(#%class*/names ,(cadr expr) 
				,(annotate (caddr expr) env) ,(map (lambda (e) (annotate e env)) (cadddr expr))
				,(let loop ([l (list-ref expr 4)])
				   (if (pair? l)
				       (cons (let ([v (car l)])
					       (if (pair? v)
						   `(,(car v) ,(annotate (cadr v) body-env))
						   v))
					     (loop (cdr l)))
				       l))
				,@(map (lambda (clause)
					 (case (car clause)
					   [(sequence) `(sequence ,@(map (lambda (e) (annotate e body-env)) (cdr clause)))]
					   [(inherit rename) clause]
					   [(public override private)
					    `(,(car clause) ,@(map (lambda (binding)
								     (if (or (symbol? binding)
									     (null? (cdr binding)))
									 binding
									 `(,(car binding) 
									   ,(annotate-named 
									     (let ([name (car binding)])
									       (if (symbol? name)
										   name
										   (car name)))
									     (cadr binding)
									     body-env))))
								   (cdr clause)))]))
				       (list-tail expr 5))))]
	    [(#%interface)
	     `(#%interface ,(map (lambda (e) (annotate e env)) (cadr expr)) ,@(cddr expr))]
	    [else 
	     (map (lambda (e) (annotate e env)) expr)]))]
	[else expr])))

   (define annotate (make-annotate #f #f))
   (define annotate-top (make-annotate #t #f))
   (define annotate-named (lambda (name expr env) ((make-annotate #t name) expr env)))

   (current-eval
    (let ([orig (current-eval)])
      (lambda (e)
	(if (and (instrumenting-enabled)
		 (with-handlers ([void (lambda (x) #f)])
		   (global-defined-value '#%with-continuation-mark)))
	    (let ([a (annotate-top (expand-defmacro e) null)])
	      ; (printf "~s~n" a)
	      (orig a))
	    ;; The empty namespace, maybe? Don't annotate.
	    (orig e)))))
   
   (current-exception-handler
    (let ([orig (current-exception-handler)])
      (lambda (x)
	(if (exn? x)
	    (let ([p (current-error-port)])
	      (display (exn-message x) p)
	      (newline p)
	      (let loop ([n (error-context-display-depth)]
			 [l (exn-debug-info x)])
		(cond
		 [(or (zero? n) (null? l)) (void)]
		 [else
		  (let ([m (car l)])
		    (fprintf p "  ~e in ~a~n" 
			     (cdr m)
			     (let ([file (car m)])
			       (if file
				   file
				   "UNKNOWN"))))
		  (loop (- n 1)
			(cdr l))]))
	      ((error-escape-handler)))
	    (orig x)))))

   (define current-file #f)

   (current-load
    (let ([load (current-load)])
      (lambda (f)
	(let ([cf current-file])
	  (dynamic-wind
	   (lambda () (set! current-file f))
	   (lambda () (load f))
	   (lambda () (set! current-file cf)))))))
   
   (debug-info-handler
    (lambda () (current-continuation-marks key)))))
 
