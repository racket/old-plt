
(invoke-open-unit
 (unit 
   (import)
   (export profiling-enabled profile-poll-interval get-profile-counts)
   
   (define key (gensym 'key))

   (define profile-thread #f)
   (define profile-key (gensym))

   (define profiling-enabled (make-parameter #f))

   (define init-profile-count 10)
   (define profile-count init-profile-count)
   (define last-profile-time (current-milliseconds))

   (define (profile-now)
     (set! profile-count (sub1 profile-count))
     (if (zero? profile-count)
	 (begin
	   (set! profile-count init-profile-count)
	   (if (and (profiling-enabled)
		    (> (abs (- (current-milliseconds) last-profile-time))
		       (profile-poll-interval)))
	       (begin
		 (set! last-profile-time (current-milliseconds))
		 #t)
	       #f))
	 #f))

   (define profile-poll-interval (make-parameter 1))

   (define profile-info (make-hash-table))

   (define (register-profile-info key)
     (let ([v (hash-table-get profile-info key)])
       (set-car! v (add1 (car v)))
       (let ([v (cdddr v)])
	 (set-car! v (cons (current-continuation-marks profile-key) (car v))))))
       
   (define (get-profile-counts)
     (hash-table-map profile-info (lambda (key val)
				    (let ([count (car val)]
					  [name (cadr val)]
					  [file (caddr val)]
					  [cmss (cadddr val)])
				      (list count name file
					    (map
					     (lambda (cms)
					       (map (lambda (k)
						      (let ([v (hash-table-get profile-info k)])
							(list (cadr v) (caddr v))))
						    cms))
					     cmss))))))

   (define (with-mark mark expr)
     `(#%with-continuation-mark
       (#%quote ,key) (#%quote (,current-file . ,mark))
       ,expr))

   (define (profile-point body name expr)
     (if (profiling-enabled)
	 (let ([key (gensym)])
	   (hash-table-put! profile-info key (list 0 (or name expr) current-file null))
	   `((#%when (,profile-now)
		(,register-profile-info (#%quote ,key)))
             (#%with-continuation-mark
	      (#%quote ,profile-key) (#%quote ,key)
	      (#%begin
	       ,@body))))
	 body))
   
   (define (make-annotate top? name)
     (lambda (expr)
       (cond
	[(symbol? expr)
	 (with-mark expr expr)]
	[(and top?
	      (pair? expr)
	      (memq (car expr)
		    '(#%define-values #%define-macro
				      #%define-id-macro
				      #%define-expansion-time
				      #%begin)))
	 ;; Can't put annotation on the outside
	 (if (eq? (car expr) '#%begin)
	     `(#%begin ,@(map annotate-top (cdr expr)))
	     `(,(car expr) ,(cadr expr) 
			   ,(with-mark expr (annotate-named
					     (and (eq? (car expr) '#%define-values)
						  (= 1 (length (cadr expr)))
						  (caadr expr))
					     (caddr expr)))))]
	[(pair? expr)
	 (with-mark
	  expr
	  (case (car expr)
	    [(#%quote) expr]
	    [(#%lambda)
	     `(#%lambda ,(cadr expr)
			,@(profile-point 
			   (map annotate (cddr expr))
			   name expr))]
	    [(#%case-lambda)
	     `(#%case-lambda 
	       ,@(map (lambda (clause)
			`(,(car clause) 
			  ,@(profile-point
			     (map annotate (cdr clause))
			     name expr)))
		      (cdr expr)))]
	    [(#%let-values #%letrec-values)
	     `(,(car expr)
	       ,(map (lambda (clause)
		       `(,(car clause) ,(annotate-named
					 (and (= (length (car clause)) 1)
					      (caar clause))
					 (cadr clause))))
		     (cadr expr))
	       ,@(map annotate (cddr expr)))]
	    [(#%set!)
	     `(#%set! ,(cadr expr) ,(annotate-named (cadr expr) (caddr expr)))]
	    [(#%begin #%begin0 #%if #%with-continuation-mark)
	     `(,(car expr) ,@(map annotate (cdr expr)))]
	    [(#%cond) expr]
	    [(#%struct)
	     (if (pair? (cadr expr))
		 `(#%struct (,(caadr expr) ,(annotate (cadadr expr))) ,@(cddr expr))
		 expr)]
	    [(#%unit)
	     `(#%unit ,(cadr expr) ,(caddr expr)
		      ,@(map annotate-top (cdddr expr)))]
	    [(#%compound-unit)
	     `(#%compound-unit
	       ,(cadr expr)
	       (link ,@(map (lambda (clause)
			      `(,(car clause) (,(annotate (caadr clause)) ,@(cdadr clause))))
			    (cdaddr expr)))
	       ,(cadddr expr))]
	    [(#%invoke-unit #%invoke-open-unit)
	     `(,(car expr) ,(annotate (cadr expr)) ,@(cddr expr))]
	    [(#%class*/names)
	     `(#%class*/names ,(cadr expr) ,(annotate (caddr expr)) ,(map annotate (cadddr expr))
			      ,(let loop ([l (list-ref expr 4)])
				 (if (pair? l)
				     (cons (let ([v (car l)])
					     (if (pair? v)
						 `(,(car v) ,(annotate (cadr v)))
						 v))
					   (loop (cdr l)))
				     l))
			      ,@(map (lambda (clause)
				       (case (car clause)
					 [(sequence) `(sequence ,@(map annotate (cdr clause)))]
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
									   (cadr binding)))))
								 (cdr clause)))]))
				     (list-tail expr 5)))]
	    [(#%interface)
	     `(#%interface ,(map annotate (cadr expr)) ,@(cddr expr))]
	    [else 
	     (map annotate expr)]))]
	[else expr])))

   (define annotate (make-annotate #f #f))
   (define annotate-top (make-annotate #t #f))
   (define annotate-named (lambda (name expr) ((make-annotate #t name) expr)))

   (current-eval
    (let ([orig (current-eval)])
      (lambda (e)
	(if (with-handlers ([void (lambda (x) #f)])
	      (global-defined-value '#%with-continuation-mark))
	    (let ([a (annotate-top (expand-defmacro e))])
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
	      (for-each
	       (lambda (m)
		 (fprintf p "  ~e in ~a~n" 
			  (cdr m)
			  (let ([file (car m)])
			    (if file
				file
				"UNKNOWN"))))
	       (exn-debug-info x))
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
 
