
(invoke-unit
 (unit 
   (import)
   (export)
   
   (define key (gensym 'key))
   
   (define (with-mark mark expr)
     `(#%with-continuation-mark
       (#%quote ,key) (#%quote ,mark)
       ,expr))
   
   (define (make-annotate top?)
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
			   ,(with-mark expr (annotate (caddr expr)))))]
	[(pair? expr)
	 (with-mark
	  expr
	  (case (car expr)
	    [(#%quote) expr]
	    [(#%lambda)
	     `(#%lambda ,(cadr expr) ,@(map annotate (cddr expr)))]
	    [(#%case-lambda)
	     `(#%case-lambda 
	       ,@(map (lambda (clause)
			`(,(car clause) ,@(map annotate (cdr clause))))
		      (cdr expr)))]
	    [(#%let-values #%letrec-values)
	     `(,(car expr)
	       ,(map (lambda (clause)
		       `(,(car clause) ,(annotate (cadr clause))))
		     (cadr expr))
	       ,@(map annotate (cddr expr)))]
	    [(#%set!)
	     `(#%set! ,(cadr expr) ,(annotate (caddr expr)))]
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
					 [(public private)
					  `(,(car clause) ,@(map (lambda (binding)
								   (if (or (symbol? binding)
									   (null? (cdr binding)))
								       binding
								       `(,(car binding) ,(annotate (cadr binding)))))
								 (cdr clause)))]))
				     (list-tail expr 5)))]
	    [(#%interface)
	     `(#%interface ,(map annotate (cadr expr)) ,@(cddr expr))]
	    [else 
	     (map annotate expr)]))]
	[else expr])))

   (define annotate (make-annotate #f))
   (define annotate-top (make-annotate #t))

   (current-eval
    (let ([orig (current-eval)])
      (lambda (e)
	(if (with-handlers ([void (lambda (x) #f)])
	      (global-defined-value '#%with-continuation-mark))
	    (let ([a (annotate-top (expand-defmacro e))])
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
		 (fprintf p "  ~e~n" m))
	       (exn-debug-info x))
	      ((error-escape-handler)))
	    (orig x)))))
   
   (debug-info-handler
    (lambda () (current-continuation-marks key)))))
 
