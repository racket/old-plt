(unit/sig zodiac:sexp^
  (import zodiac:misc^
    zodiac:structures^
    (z : zodiac:reader-structs^)
    zodiac:interface^)

  (define identity (lambda (x) x))

  (define structurize-syntax
    (opt-lambda (expr source (marks '()) (table #f))
      (let ((origin (make-origin 'non-source 'never-mind))
	     (start (zodiac-start source))
	     (finish (zodiac-finish source)))
	(letrec
	  ((structurize
	     (lambda (expr)
	       (cond
		 ((zodiac? expr) expr)
		 ((and table
		    (hash-table-get table expr (lambda () #f)))
		   =>
		   (lambda (cached-input)
		     cached-input))
		 ((pair? expr)
		   (let loop ((expr expr) (rev-seen '()) (length 0))
		     (cond
		       ((pair? expr)
			 (loop (cdr expr)
			   (cons (structurize (car expr)) rev-seen)
			   (add1 length)))
		       ((null? expr)
			 (z:make-list origin start finish
			   (reverse rev-seen)
			   length '()))
		       (else
			 (z:make-improper-list origin start finish
			   (reverse
			     (cons (structurize expr) rev-seen))
			   (add1 length)
			   (make-period start)
			   '())))))
		 ((symbol? expr)
		   (z:make-symbol
		     origin start finish expr expr marks))
		 ((null? expr)
		   (z:make-list origin start finish '() 0 marks))
		 ((string? expr)
		   (z:make-string origin start finish expr))
		 ((number? expr)
		   (z:make-number origin start finish expr))
		 ((boolean? expr)
		   (z:make-boolean origin start finish expr))
		 ((type-symbol? expr)
		   (z:make-type-symbol origin start finish expr))
		 ((char? expr)
		   (z:make-char origin start finish expr))
		 (else
		   (z:make-list origin start finish
		     (list
		       (z:make-symbol origin start finish
			 'quote 'quote '(-1))
		       expr)
		     2 marks))))))
	  (structurize expr)))))

  (define set-macro-origin
    (lambda (parsed-term head-sexp)
      (set-zodiac-origin! parsed-term
	(make-origin 'macro
	  (if (z:symbol? head-sexp)
	    head-sexp
	    (internal-error 'set-macro-origin
	      "Shouldn't get ~s here" head-sexp))))))

  (define sexp->raw
    (opt-lambda (expr (table #f))
      (cond
	((z:scalar? expr)
	  (if (z:box? expr)
	    (box (sexp->raw (z:read-object expr) table))
	    (z:read-object expr)))
	((z:sequence? expr)
	  (let ((output
		  (let ((objects (map (lambda (s)
					(sexp->raw s table))
				   (z:read-object expr))))
		    (cond
		      ((z:list? expr) objects)
		      ((z:improper-list? expr)
			(let loop ((objects objects))
			  (if (or (null? objects) (null? (cdr objects)))
			    (internal-error expr
			      "Invalid ilist in sexp->raw")
			    (if (null? (cddr objects))
			      (cons (car objects) (cadr objects))
			      (cons (car objects) (loop (cdr objects)))))))
		      ((z:vector? expr)
			(apply vector objects))))))
					;	      (printf "Created entry for ~s~n" output)
	    (when table
	      (hash-table-put! table output expr))
	    output))
	(else
	  expr))))

					; ----------------------------------------------------------------------

  (define syntax-null?
    (lambda (l)
      (and (z:list? l)
	(= 0 (z:sequence-length l)))))
    
  (define syntax-car
    (lambda (l)
      (cond
	((or (z:list? l) (z:improper-list? l))
	  (let ((object (expose-list l)))
	    (if (null? object)
	      (internal-error l "Empty list for syntax-car")
	      (car object))))
	(else (internal-error l "Not a list for syntax-car")))))

  (define syntax-cdr
    (lambda (l)
      (cond
	((z:list? l)
	  (let ((object (expose-list l))
		 (length (z:sequence-length l)))
	    (if (zero? length)
	      (internal-error l "Empty list for syntax-cdr")
	      (let ((result (cdr object)))
		(z:make-list (zodiac-origin l)
		  (if (null? result) (zodiac-finish l)
		    (zodiac-start (car result)))
		  (zodiac-finish l)
		  result
		  (- length 1) '())))))
	((z:improper-list? l)
	  (let ((object (expose-list l))
		 (length (z:sequence-length l)))
	    (case length
	      ((0 1) (internal-error l "Improper list length is 0 or 1"))
	      ((2) (cadr object))
	      (else
		(let ((result (cdr object)))
		  (z:make-improper-list (zodiac-origin l)
		    (zodiac-start l) (zodiac-finish l)
		    result
		    (- length 1)
		    (z:improper-list-period l) '()))))))
	(else (internal-error l "Not a list for syntax-cdr")))))

  (define syntax-map
    (case-lambda
      ((f l)
	(if (z:list? l)
	  (let ((object (expose-list l))
		 (length (z:sequence-length l)))
	    (z:make-list (zodiac-origin l)
	      (zodiac-start l) (zodiac-finish l)
	      (map f object) length '()))
	  (internal-error l "Not a list for syntax-map")))
      ((f l1 l2)
	(if (and (z:list? l1) (z:list? l2))
	  (let ((object-1 (expose-list l1))
		 (object-2 (expose-list l2))
		 (length-1 (z:sequence-length l1))
		 (length-2 (z:sequence-length l2)))
	    (if (= length-1 length-2)
	      (z:make-list (zodiac-origin l1)
		(zodiac-start l1) (zodiac-finish l1)
		(map f object-1 object-2) length-1 '())
	      (internal-error l1 "Not of same length as ~s in syntax-map"
		l2)))
	  (if (z:list? l1)
	    (internal-error l2 "Not a list for syntax-map")
	    (internal-error l1 "Not a list for syntax-map"))))))

					; ----------------------------------------------------------------------

  (define new-mark
    (let ((m 0))
      (lambda ()
	(set! m (+ m 1))
	m)))

  (define mark-expression
    (lambda (mark)
      (lambda (expr)
	(cond
	  ((z:list? expr)
	    (z:set-list-marks! expr
	      (add/remove-mark (z:list-marks expr) mark))
	    expr)
	  ((z:symbol? expr)
	    (z:make-symbol (zodiac-origin expr)
	      (zodiac-start expr) (zodiac-finish expr)
	      (z:read-object expr) (z:symbol-orig-name expr)
	      (add/remove-mark (z:symbol-marks expr) mark)))
	  (else expr)))))

  (define carl car)

  (define add/remove-mark
    (lambda (marks m)
      (let loop
	((marks marks))
	(if (null? marks) (list m)
	  (let ((a (carl marks)) (d (cdr marks)))
	    (if (= a m) d
	      (cons a (loop d))))))))

  (define expose-list
    (lambda (l)
      (cond
	((z:list? l)
	  (let ((marks (z:list-marks l))
		 (object (z:read-object l)))
	    (if (null? marks)
	      object
	      (let
		((object
		   (let loop ((marks marks) (object object))
		     (if (null? marks) object
		       (loop (cdr marks)
			 (map (mark-expression (carl marks)) object))))))
		(z:set-read-object! l object)
		(z:set-list-marks! l '())
		object))))
	((z:improper-list? l)
	  (let ((marks (z:improper-list-marks l))
		 (object (z:read-object l)))
	    (if (null? marks)
	      object
	      (let
		((object
		   (let loop ((marks marks) (object object))
		     (if (null? marks) object
		       (loop (cdr marks)
			 (map (mark-expression (carl marks)) object))))))
		(z:set-read-object! l object)
		(z:set-improper-list-marks! l '())
		object))))
	(else
	  (internal-error l "Not appropriate for expose-list")))))

  )
