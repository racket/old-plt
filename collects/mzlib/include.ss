
(module include mzscheme
  (require-for-syntax (lib "stx.ss" "syntax"))

  (define-syntax include-at/relative-to
    (lambda (stx)
      ;; Parse the file name
      (let-values ([(ctx loc file)
		    (syntax-case* stx (build-path) module-or-top-identifier=?
		      [(_ ctx loc fn)
		       (string? (syntax-e (syntax fn)))
		       (values (syntax ctx) (syntax loc) (syntax-e (syntax fn)))]
		      [(_ ctx loc (build-path elem1 elem ...))
		       (andmap
			(lambda (e)
			  (or (string? (syntax-e e))
			      (and (identifier? e)
				   (or
				    (module-identifier=? e (quote-syntax up))
				    (module-identifier=? e (quote-syntax same))))))
			(syntax->list (syntax (elem1 elem ...))))
		       (values 
			(syntax ctx)
			(syntax loc)
			(apply build-path (syntax-object->datum (syntax (elem1 elem ...)))))])])
	;; Complete the file name
	(let ([c-file
	       (if (complete-path? file)
		   file
		   (path->complete-path
		    file
		    (cond
		     ;; Src of include expression is a path?
		     [(and (string? (syntax-source loc))
			   (complete-path? (syntax-source loc)))
		      (let-values ([(base name dir?) 
				    (split-path (syntax-source loc))])
			(if dir?
			    (syntax-source loc)
			    base))]
		     ;; Load relative?
		     [(current-load-relative-directory)]
		     ;; Current directory
		     [(current-directory)]
		     [else (raise-syntax-error
			    #f
			    "can't determine a base path"
			    stx)])))])
	  ;; Open the included file
	  (let ([p (with-handlers ([not-break-exn?
				    (lambda (exn)
				      (raise-syntax-error
				       #f
				       (format
					"can't open include file (~a)"
					(if (exn? exn)
					    (exn-message exn)
					    exn))
				       stx
				       c-file))])
		     (open-input-file c-file))])
	    (port-count-lines! p)
	    ;; Read expressions from file
	    (let ([content
		   (let loop ()
		     (let ([r (with-handlers ([not-break-exn?
					       (lambda (exn)
						 (raise-syntax-error
						  #f
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
			    ctx
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

  (define-syntax include
    (lambda (stx)
      (syntax-case stx ()
	[(_ fn)
	 ;; Check form of fn:
	 (syntax-case* (syntax fn) (build-path) module-or-top-identifier=?
	   [fn
	    (string? (syntax-e (syntax fn)))
	    'ok]
	   [(build-path elem1 elem ...)
	    (andmap
	     (lambda (e)
	       (or (string? (syntax-e e))
		   (and (identifier? e)
			(or
			 (module-identifier=? e (quote-syntax up))
			 (module-identifier=? e (quote-syntax same))))))
	     (syntax->list (syntax (elem1 elem ...))))
	    'ok]
	   [_else (raise-syntax-error #f "bad syntax" stx (syntax fn))])
	 ;; Expand to include-at/relative-to:
	 (with-syntax ([_stx stx])
	   (syntax/loc stx (include-at/relative-to _stx _stx fn)))])))
  
  (provide include
	   include-at/relative-to))

		 
			   
	      
		     
