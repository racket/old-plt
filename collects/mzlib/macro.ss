
(reference-library "letplus.ss")

; let form where, instead of a list of name-value pairs, just
; provide a list of names and they are bound to a sequence of
; numbers (starting with 1)
(define-macro let-enumerate 
  (lambda (name-lists . body)
    (cons 'let
	  (cons
	   (let enum-all ((lists name-lists))
	     (if (null? lists)
		 '()
		 (append (let enum ((names (car lists)) (pos 1))
			   (if (null? names)
			       '()
			       (cons (list (car names) pos)
				     (enum (cdr names) (+ pos 1)))))
			 (enum-all (cdr lists)))))
	   body))))

(define-macro catch-errors 
  (lambda (displayer failure . body)
    (let ([orig-displayer (gensym)]
	  [orig-escaper (gensym)]
	  [err (gensym)]
	  [my-error (gensym)]
	  [this-displayer (gensym)])
      `(let* ([,orig-displayer (error-display-handler)]
	      [,orig-escaper (error-escape-handler)]
	      [,this-displayer ,displayer])
	 (dynamic-wind
	  (lambda () #f)
	  (lambda ()
	    (let/ec ,err
		   (if ,this-displayer
		       (error-display-handler ,this-displayer))
		   (error-escape-handler
		    (lambda () (,err (,failure))))
		   ,@body))
	  (lambda () 
	    (error-display-handler ,orig-displayer)
	    (error-escape-handler ,orig-escaper)))))))

(define-macro send*
  (lambda (x . rest)
    (let ([g (gensym "send*")])
      `(let ([,g ,x])
	 ,@(map (lambda (x) `(send ,g ,@x))
		rest)))))

(define-macro class*-asi
  (lambda (super interfaces . body)
    (let ([args (gensym)]
	  [super-init 'super-init])
      `(class* ,super ,interfaces ,args
	  ,@body
	  (sequence
	    (apply ,super-init ,args))))))

(define-macro class-asi
  (lambda (super . rest)
    `(class*-asi ,super () ,@rest)))

(define-macro opt-lambda 
  (lambda (args . body)
    (let* ([mk-code (lambda () (list* 'opt-lambda args body))]
	   [f (gensym)]
	   [required
	    (let loop ([args args])
	      (if (and (pair? args)
		       (symbol? (car args)))
		  (cons (car args) (loop (cdr args)))
		  '()))]
	   [not-required-with-defaults
	    (let loop ([args args])
	      (if (and (pair? args)
		       (symbol? (car args)))
		  (loop (cdr args))
		  args))]
	   [not-required
	    (let loop ([args not-required-with-defaults])
	      (if (pair? args)
		  (if (pair? (car args))
		      (let ([name (caar args)])
			(if (symbol? name)
			    (cons name (loop (cdr args)))
			    (raise-syntax-error 
			     'opt-lambda
			     "all argument names must be symbols"
			     (mk-code))))
		      (raise-syntax-error 'opt-lambda
					  "all required args must come first"
					  (mk-code)))
		  (if (or (null? args) (symbol? args))
		      args
		      (raise-syntax-error 'opt-lambda 
					  "all argument names must be symbols"
					  (mk-code)))))]
	   [defaults
	     (let loop ([args not-required-with-defaults])
	       (if (pair? args)
		   (let ([v (cdar args)])
		     (if (and (pair? v) (null? (cdr v)))
			 (cons (car v) (loop (cdr args)))
			 (raise-syntax-error 
			  'opt-lambda
			  "only one default value allowed per argument"
			  (mk-code))))
		   ()))])
      `(letrec ([,f
		 (case-lambda
		  ,@(let loop ([required required]
			       [not-required not-required]
			       [defaults defaults])
		      (if (not (pair? not-required))
			  (list `(,(append required not-required) ,@body))
			  (cons `(,required
				  ,(cons f (append required 
						   (list (car defaults)))))
				(loop (append required (list (car not-required)))
				      (cdr not-required)
				      (cdr defaults))))))])
	 ,f))))

; make-spawner "returns" a procedure taking the arguments
;  args and spawning a new process to evaluate body
(define-macro make-spawner 
  (lambda (spawn-opts args . body)
    (let ([opt (gensym)])
      `(let ([,opt ,spawn-opts])
	 (lambda args
	   ((apply
	     make-eval
	     ,opt)
	    ,(list 'quasiquote
		   (list
		    'thread
		    (list 'lambda '()
			  (list 'apply
				(list* 'lambda args
				       body)
				(list 'list
				      (list 'unquote-splicing 'args))))))))))))

; recur is another name for 'let' in a named let
(define-macro recur 
  (lambda (name args . body) `(let ,name ,args ,@body)))

; define a recursive value
(define-macro rec
  (lambda (x rest)
    (if (symbol? x)
	`(letrec ([,x ,rest])
	   ,x)
	(syntax-error 'rec "identifier must be a symbol" 
		      (list 'rec x rest) x))))

; Another let-like form. We perform no checking.
(define-macro local 
  (lambda (defines . body)
    `(let () ,@defines ,@body)))

(define-macro define-virtual-struct 
  (lambda (name/&super fields)
    (if (and (list? fields)
	     (andmap symbol? fields)
	     (or (symbol? name/&super)
		 (and (list? name/&super)
		      (= (length name/&super) 2)
		      (andmap symbol? name/&super))))
	(let ((symbol-append
	       (lambda args
		 (string->symbol (apply string-append
					(map symbol->string args)))))
	      (struct-name
	       (if (pair? name/&super) (car name/&super) name/&super)))
	  (let ((constructor (symbol-append 'make- struct-name))
		(type (symbol-append 'struct: struct-name))
		(selectors (map (lambda (field)
				  (symbol-append struct-name '- field))
				fields))
		(mutators (map (lambda (field)
				 (symbol-append 'set- struct-name '- field '!))
			       fields)))
	    `(define-some (,type ,@selectors ,@mutators)
	       (define-struct ,name/&super ,fields))))
	(raise-syntax-error 'define-virtual-struct "syntax error"
			    (list 'define-virtual-struct name/&super fields)))))


(define-macro define-struct-tree
  (letrec
      ((check-syntax
	(lambda (localroot fields tree)
	  (and (symbol? localroot)
	       (list? fields)
	       (andmap symbol? fields)
	       (andmap
		(lambda (subtree)
		  (and (list? subtree)
		       (<= 2 (length subtree))
		       (check-syntax (car subtree)
				     (cadr subtree)
				     (cddr subtree))))
		tree))))
       (generate-tree
	(lambda (root tree)
	  (map (lambda (child)
		 (let ((tag (car child)))
		   (generate-node tag 
				  `(,tag ,root) 
				  (cadr child) 
				  (cddr child))))
	       tree)))
       (generate-node (lambda (tag root fields tree)
			`(begin
			   (define-struct ,root ,fields)
			   ,@(generate-tree (struct: tag) tree))))
       (struct: (lambda (s) 
		  (string->symbol (string-append "struct:" 
						 (symbol->string s))))))
    (lambda (root fields . tree)
      (unless (and (or (symbol? root)
		       (and (pair? root)
			    (pair? (cdr root))
			    (null? (cddr root))
			    (symbol? (car root))))
		   (check-syntax 'root-dummy fields tree))
	      (raise-syntax-errror
	       'define-struct-tree
	       "syntax error"
	       (list* 'define-struct-tree root fields tree)))
      (generate-node (if (symbol? root) root (car root)) root fields tree))))
