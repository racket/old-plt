
(module docprovide mzscheme
  (require-for-syntax "private/doctable.ss")

  (define-syntax (provide-and-document stx)

    (define (add-prefix prefix rows)
      (map (lambda (row)
	     (cons (car row)
		   (map
		    (lambda (x) 
		      (cons prefix x))
		    (cdr row))))
	   rows))

    (define (remove-prefixes rows)
      (map (lambda (row)
	     (cons (car row)
		   (map cdr (cdr row))))
	   rows))

    (syntax-case stx ()
      [(_ label row ...)
       (begin
	 (unless (identifier? (syntax label))
	   (raise-syntax-error
	    'provide-and-document
	    "label is not an identifier"
	    stx
	    (syntax label)))
	 (let ([rows (map (lambda (row)
			    (syntax-case row ()
			      [(header proc ...)
			       (string? (syntax-e (syntax header)))
			       (begin
				 ;; check form:
				 (map (lambda (proc)
					(syntax-case proc ()
					  [(name type-sexpr doc-string ...)
					   (and (identifier? (syntax name))
						(andmap (lambda (s) (string? (syntax-e s)))
							(syntax->list (syntax (doc-string ...)))))
					   'ok]))
				      (syntax->list (syntax (proc ...))))
				 (add-prefix #f (list (syntax-object->datum row))))]
			      [(all-from tag path label)
			       (eq? 'all-from (syntax-e (syntax all-from)))
			       (let ([tag (syntax tag)]
				     [label (syntax label)]
				     [path (syntax-object->datum (syntax path))])
				 (unless (identifier? tag)
				   (raise-syntax-error
				    'provide-and-document
				    "prefix tag is not an identifier"
				    stx
				    tag))
				 (unless (identifier? label)
				   (raise-syntax-error
				    'provide-and-document
				    "label is not an identifier"
				    stx
				    label))
				 (let ([mod ((current-module-name-resolver) path #f #f)])
				   ;; Execute syntax part at top-level:
				   (dynamic-require mod (void))
				   ;; Extract documentation via top-level:
				   (let ([docs ((dynamic-require-for-syntax 
						 '(lib "doctable.ss" "syntax" "private") 
						 'lookup-documentation)
						mod
						(syntax-e label))])
				     (unless docs
				       (raise-syntax-error
					'provide-and-document
					"could not find provided documentation"
					stx
					row))
				     (add-prefix tag docs))))]))
			  (syntax->list (syntax (row ...))))]
	       [imports (apply
			 append
			 (map (lambda (row)
				(syntax-case row ()
				  [(header . _)
				   (string? (syntax-e (syntax header)))
				   null]
				  [(all-from tag path label)
				   (list (syntax (require (prefix tag path))))]))
			      (syntax->list (syntax (row ...)))))])
	   ;; Collapse rows for a section name:
	   (let ([rows (let loop ([rows (apply append rows)])
			 (if (null? rows)
			     null
			     (let ([rest (loop (cdr rows))])
			       (let ([a (assoc (caar rows) rest)])
				 (if a
				     (cons (cons (caar rows)
						 (append (cdar rows)
							 (cdr a)))
					   (let loop ([l rest])
					     (cond
					      [(null? l) null]
					      [(equal? (caar l) (caar rows))
					       (cdr l)]
					      [else (cons (car l) (loop (cdr l)))])))
				     (cons (car rows) rest))))))])
	     ;; Extract procs and eliminate duplicates
	     (let ([procs (let ([ht (make-hash-table)])
			    (for-each
			     (lambda (proc-line)
			       (hash-table-put! ht (cadr proc-line) (cons (car proc-line)
									  (cadr proc-line))))
			     (apply append (map cdr rows)))
			    (hash-table-map ht (lambda (key val) val)))])
	       (with-syntax ([procs (map (lambda (proc)
					   (if (car proc)
					       ;; Prefixed:
					       `(rename ,(string->symbol (format "~a~a" 
										 (syntax-e (car proc))
										 (cdr proc)))
							,(cdr proc))
					       ;; Plain
					       (cdr proc)))
					 procs)]
			     [(import ...) imports]
			     [src (datum->syntax-object stx 'source)]
			     [rows (remove-prefixes rows)])
		 (syntax/loc stx
		   (begin
		     import ...
		     (provide . procs)
		     (define-syntaxes ()
		       (begin
			 (register-documentation (quote-syntax src) 'label 'rows)
			 (values))))))))))]))

  (define (lookup-documentation path label)
    (let ([mod ((current-module-name-resolver) path #f #f)])
      (dynamic-require mod (void))
      ((dynamic-require-for-syntax 
	'(lib "doctable.ss" "syntax" "private")
	'lookup-documentation)
       mod
       label)))
       
  (provide provide-and-document
	   lookup-documentation))

			      