
(module include mzscheme

  (define-syntax include
    (lambda (stx)
      ;; Parse the file name
      (let ([file
	     (syntax-case* stx (build-path) (lambda (a b)
					      (eq? (syntax-e a)
						   (syntax-e b)))
	       [(_ fn)
		(string? (syntax-e (syntax fn)))
		(syntax-e (syntax fn))]
	       [(_ (build-path elem1 elem ...))
		(andmap
		 (lambda (e)
		   (or (string? (syntax-e e))
		       (module-identifier=? e (quote-syntax up))
		       (module-identifier=? e (quote-syntax same))))
		 (syntax->list (syntax (elem1 elem ...))))
		(apply build-path (syntax-object->datum (syntax (elem1 elem ...))))])])
	;; Complete the file name
	(let ([c-file
	       (if (complete-path? file)
		   file
		   (path->complete-path
		    file
		    (cond
		     ;; Src of include expression is a path?
		     [(and (string? (syntax-source stx))
			   (complete-path? (syntax-source stx)))
		      (let-values ([(base name dir?) 
				    (split-path (syntax-source stx))])
			(if dir?
			    (syntax-source stx)
			    base))]
		     ;; Load relative?
		     [(current-load-relative-directory)]
		     ;; Current directory
		     [(current-directory)]
		     [else (raise-syntax-error
			    'include
			    "can't determine a base path"
			    stx)])))])
	  ;; Open the included file
	  (let ([p (with-handlers ([not-break-exn?
				    (lambda (exn)
				      (raise-syntax-error
				       'include
				       (format
					"can't open include file (~a)"
					(if (exn? exn)
					    (exn-message exn)
					    exn))
				       stx
				       c-file))])
		     (open-input-file c-file))])
	    ;; Read expressions from file
	    (let ([content
		   (let loop ()
		     (let ([r (with-handlers ([not-break-exn?
					       (lambda (exn)
						 (raise-syntax-error
						  'include
						  (format
						   "read error (~a)"
						   (if (exn? exn)
						       (exn-message exn)
						       exn))
						  stx))])
				(read-syntax c-file p))])
		       (if (eof-object? r)
			   null
			   (cons r (loop)))))])
	      ;; Preserve src info for content, but set its
	      ;; lexical context to be that of the include expression
	      (let ([lexed-content
		     (let loop ([content content])
		       (cond
			[(pair? content)
			 (cons (loop (car content))
			       (loop (cdr content)))]
			[(null? content) null]
			[else
			 (let ([v (syntax-e content)])
			   (datum->syntax-object
			    stx
			    (cond
			     [(pair? v) 
			      (loop v)]
			     [(vector? v)
			      (list->vector (loop (vector->list v)))]
			     [(box? v)
			      (box (loop (unbox v)))]
			     [else
			      v])
			    content))]))])
		(datum->syntax-object
		 (quote-syntax here)
		 `(begin ,@lexed-content)
		 stx))))))))

  (provide include))

		 
			   
	      
		     
