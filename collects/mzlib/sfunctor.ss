
(require-library "constant.ss")

(define-struct signature (exports))
(define-struct sigfunctor (functor exports imports))
(define-struct sigfunctor-set (links))

(define-macro define-signature
  (lambda (name . id-list)
    (unless (and (symbol? name)
		 (list? id-list)
		 (andmap (lambda (v)
			   (or (symbol? v)
			       (and (list? v)
				    (= (length v) 3)
				    (eq? (car v) 'struct)
				    (symbol? (cadr v))
				    (list? (caddr v))
				    (andmap symbol? (caddr v)))))
			 id-list))
	    (raise (make-exn:syntax "define-signature: bad syntax"
				    ((debug-info-handler))
				    (list* 'define-signature
					   name
					   id-list))))
    `(define ,name
       (make-signature (quote ,(apply append
				      (map (lambda (v)
					     (if (symbol? v)
						 (list v)
						 (let ([name (symbol->string (cadr v))]
						       [fields (map symbol->string
								    (caddr v))]
						       [+ string-append])
						   (map string->symbol
							(append
							 (list 
							  (+ "make-" name)
							  (+ name "?")
							  (+ "struct:" name))
							 (map
							  (lambda (f)
							    (+ name "-" f))
							  fields)
							 (map
							  (lambda (f)
							    (+ "set-" name "-" f "!"))
							  fields))))))
					   id-list)))))))

(define-macro define-sigfunctor
  (lambda (name-&-signature import-names . body)
    (let-values ([(renames body) (if (and (pair? body)
					  (pair? (car body))
					  (eq? (caar body) 'rename))
				     (values (cdar body) (cdr body))
				     (values '() body))])
      (unless (and (list? name-&-signature)
		   (= 2 (length name-&-signature))
		   (andmap symbol? name-&-signature)
		   (list? import-names)
		   (pair? import-names)
		   (eq? (car import-names) 'import)
		   (andmap symbol? import-names)
		   (andmap (lambda (p)
			     (and (list? p) 
				  (= 2 (length p))
				  (symbol? (car p))
				  (symbol? (cadr p))))
			   renames))
	      (raise (make-exn:syntax "define-sigfunctor: bad syntax"
				      ((debug-info-handler))
				      (list* 'define-sigfunctor 
					     name-&-signature import-names body))))
      (let ([signature (global-defined-value (cadr name-&-signature))]
	    [imports (map global-defined-value (cdr import-names))])
      `(define ,(car name-&-signature)
	 (make-sigfunctor
	  (unit
	   (import ,@(apply append (map (lambda (import i-name)
					  (let ([name (symbol->string i-name)])
					    (map
					     (lambda (s)
					       (string->symbol
						(string-append name ":"
							       (symbol->string s))))
					     (signature-exports import))))
					imports (cdr import-names))))
	   (export ,@(map (lambda (name)
			    (or (let loop ([renames renames])
				  (cond
				   [(null? renames) #f]
				   [(eq? (cadar renames) name) (car renames)]
				   [else (loop (cdr renames))]))
				name))
			  (signature-exports signature)))
	   ,@body)
	  (quote ,(signature-exports signature))
	  (quote ,(map signature-exports imports))))))))

(define-macro define-sigfunctor-set
  (lambda (name . mapping)
    (unless (and (symbol? name)
		 (andmap (lambda (link)
			   (and (list? link)
				(andmap symbol? link)))
			 mapping))
	    (raise (make-exn:syntax "define-sigfunctor-set: bad syntax"
				    ((debug-info-handler))
				    (list* 'define-sigfunctor-set
					   name
					   mapping))))
    `(define ,name
       (make-sigfunctor-set
	(quote ,mapping)))))

(define-macro define-compound-sigfunctor
  (lambda (name-&-signature imports sub-functors exports)
    (unless (and (list? name-&-signature)
		 (= 2 (length name-&-signature))
		 (andmap symbol? name-&-signature )
		 (list? imports)
		 (pair? imports)
		 (eq? (car imports) 'import)
		 (andmap symbol? imports)
		 (list? sub-functors)
		 (pair? sub-functors)
		 (eq? (car sub-functors) 'with)
		 (andmap (lambda (sf)
			   (or (symbol? sf)
			       (and (list? sf)
				    (not (null? sf))
				    (andmap symbol? sf))))
			 (cdr sub-functors))
		 (list? exports)
		 (pair? exports)
		 (eq? (car exports) 'export)
		 (andmap (lambda (sf)
			   (or (and (symbol? sf)
				    (member sf (apply append
						      (map (lambda (x)
							     (if (pair? x)
								 (list (car x))
								 (cons 
								  x
								  (map car
								       (sigfunctor-set-links
									(global-defined-value x))))))
							   (cdr sub-functors)))))
			       (and (list? sf)
				    (< 1 (length sf))
				    (andmap symbol? sf))))
			 (cdr exports)))
	    (raise (make-exn:syntax "define-compound-sigfunctor: bad syntax"
				    ((debug-info-handler))
				    (list name-&-signature imports sub-functors exports))))
    (let* ([exports (let loop ([l (cdr exports)])
		      (cond
		       [(null? l) '()]
		       [(symbol? (car l))
			(let ([v (global-defined-value (car l))])
			  (if (sigfunctor? v)
			      (cons (cons (car l) (sigfunctor-exports v))
				    (loop (cdr l)))
			      (append (loop (map car (sigfunctor-set-links v)))
				      (loop (cdr l)))))]
		       [else (cons (car l) (loop (cdr l)))]))]
	   [sub-functors (let loop ([l (cdr sub-functors)])
			   (cond
			    [(null? l) '()]
			    [(symbol? (car l))
			     (append (sigfunctor-set-links (global-defined-value (car l)))
				     (loop (cdr l)))]
			    [else (cons (car l) (loop (cdr l)))]))]
	   [signature (global-defined-value (cadr name-&-signature))]
	   [imports (map global-defined-value (cdr imports))]
	   [export-names (apply append (map cdr exports))])
      (for-each (lambda (exported)
		  (when (not (member exported export-names))
			(error "define-compound-sigfunctor: signature requires ~s to be exported" 
			       exported)))
		(signature-exports signature))
      `(define ,(car name-&-signature)
	 (let ([sub-functors (list ,@(map car sub-functors))]
	       [sf-imports (list ,@(map (lambda (sf)
					  `(list ,@(cdr sf)))
					sub-functors))])
	   (for-each (lambda (sf imports)
		       (let ([iis (sigfunctor-imports sf)])
			 (if (= (length imports) (length iis))
			     (for-each
			      (lambda (expect given)
				(when (not (equal? expect given))
				      (error "define-compound-sigfunctor: imported functor does not have expected signature; expected: ~s; given: ~s"
				      expect
				      given)))
			      iis
			      (map sigfunctor-exports imports))
			     (error "define-compound-sigfunctor: wrong number of imported functors"))))
		     sub-functors
		     sf-imports)
	   (make-sigfunctor
	    (compound-unit
	     (import ,@(apply append (map signature-exports imports)))
	     (link ,@(map (lambda (sf)
			    `(,(car sf) [(sigfunctor-functor ,(car sf))
					 ,@(map
					    (lambda (name)
					      (let ([signature (global-defined-value name)])
						`(,name ,@(sigfunctor-exports signature))))
					    (cdr sf))]))
			  sub-functors))
	     (export ,@exports))
	    (quote ,(signature-exports signature))
	    (quote ,(map signature-exports imports))))))))


(define sigfunctor->functor sigfunctor-functor)

#|

(define-signature A j k l)
(define-sigfunctor (A@ A) 
  (import) 
  (define j 8) 
  (define k 9) 
  (define l 6))

(define-signature B w)
(define-sigfunctor (B@ B) 
  (import A) 
  (define (w) (+ A:j A:k A:l)))

(define-compound-sigfunctor (X@ B) 
  (import) 
  (with (B@ A@) (A@)) 
  (export (B@ w)))

(define-compound-sigfunctor (Y@ A) 
  (import) 
  (with (B@ A@) (A@)) 
  (export A@))

(define-sigfunctor-set AB@@
  (B@ A@)
  (A@))

(define-compound-sigfunctor (Z@ B) 
  (import) 
  (with AB@@)
  (export AB@@))

|#

(define-id-macro invoke-functor 'invoke-unit)
(define-id-macro invoke-open-functor 'invoke-open-unit)
