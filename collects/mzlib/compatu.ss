
;
; Some functions that are commonly provided in other Scheme implementations
;

(reference-library "refer.ss")

(reference-library "functios.ss")
(reference-library "compats.ss")

(define mzlib:compat@ (reference-library-unit/sig "compatr.ss"))

(define-macro defmacro
  (lambda (name args . body)
    `(define-macro ,name (lambda ,args ,@body))))

(define-macro letmacro
  (lambda (name args mbody . body)
    `(let-macro ,name (lambda ,args ,mbody)
		,body)))

(define-macro make-class* 
  (lambda (supers . rest)
    (let ([first (if (pair? rest) (car rest) #f)]
	  [mk-code (lambda () (list* 'make-class* supers rest))])
      (if (and (pair? first)
	       (pair? (car first)))
	  ; Old "new" syntax
	  `(make-class* ,supers ,@(car rest) ,@(cdr rest))
	  (let-values ([(ivars init)
			(let loop ([l rest])
			  (cond
			   [(null? l)
			    (values '()
				    (if (null? supers)
					'void
					`(lambda args
					   ,@(map (lambda (x)
						    (list
						     'apply
						     (string->symbol
						      (string-append (symbol->string (car x))
								     "-init"))
						     'args))
						  supers))))]
			   [(not (pair? l))
			    (raise-syntax-error '|make-class* (obsolete: use class or class* instead)|
						  "bad syntax (improper form)"
						  (mk-code))]
			   [(and (pair? (car l))
				 (member (caar l) '(public
						    private
						    local
						    inherit
						    rename
						    share
						    inherit-from
						    rename-from
						    share-from)))
			    (let-values ([(ivars init) (loop (cdr l))])
					(values (cons (car l) ivars) init))]
			   [(not (null? (cdr l)))
			    (raise-syntax-error '|make-class* (obsolete: use class or class* instead)|
						"bad syntax (multiple init expressions)"
						(mk-code))]
			   [else
			    (values '() (car l))]))]
		       [(super-init super-expr)
			(cond
			 [(zero? (length supers)) (values 'super-init null)]
			 [(= (length supers) 1) (let ([sname (caar supers)]
						      [expr (cadar supers)])
						  (values
						   (string->symbol
						    (string-append
						     (symbol->string sname)
						     "-init"))
						   expr))]
			 [else
			  (raise-syntax-error '|make-class* (obsolete: use class or class* instead)|
						"multiple inheritance no longer supported"
						(mk-code))])])
		      (let ([gen (gensym)])
			`(class*/names (this ,super-init) ,super-expr () ,gen
				 ,@ivars
				 (private
				  ,@(map (lambda (s)
					   `(,(string->symbol
					       (string-append "apply-" 
							      (symbol->string (car s))
							      "-init"))
					     (lambda args
					       (#%apply ,(string->symbol
							  (string-append (symbol->string 
									  (car s))
									 "-init"))
							(let loop ([l args])
							  (if (#%null? (cdr l))
							      (#%car l)
							      (#%cons (#%car l) 
								      (loop (#%cdr l)))))))))
					 supers))
				 (sequence
				   (apply ,init
					  ,gen)))))))))

(define-macro make-class 
  (lambda (super . rest)
    `(make-class* ((super ,super)) ,@rest)))
