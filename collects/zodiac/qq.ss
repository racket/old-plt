(define qq-base-level 0)

(define qq-vocab (make-vocabulary))

(add-micro-form 'quasiquote scheme-vocabulary
  (let* ((kwd '(quasiquote))
	  (in-pattern '(quasiquote template))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((template (pat:pexpand 'template p-env kwd))
		   (new-qq-level
		     (add1 qq-base-level)))
	      (put-attribute attributes 'qq-level new-qq-level)
	      (put-attribute attributes 'qq-changed? #f)
	      (let ((result (expand-expr template
			      env attributes qq-vocab)))
		(put-attribute attributes 'qq-level (sub1 new-qq-level))
		(if (get-attribute attributes 'qq-changed?)
		  (expand-expr result env attributes vocab)
		  (expand-expr
		    (structurize-syntax
		      (list 'quote template)
		      expr)
		    env attributes vocab))))))
	(else
	  (static-error expr "Malformed quasiquote"))))))

(add-sym-micro qq-vocab
  (lambda (expr env attributes vocab)
    expr))

(define-struct qq-seq-entry (orig expanded changed? uq-type))

(define common-sequence-handler
  (lambda (extractor no-uq@-constructor uq@-finalizer uq@-cons uq@-append)
    (lambda (expr env attributes vocab)
      (let loop ((items (extractor expr)) (result '()) (changed? #f)
		  (no-uq-splicings? #t))
	(if (null? items)
	  (if changed?
	    (begin
	      (put-attribute attributes 'qq-changed? #t)
	      (if no-uq-splicings?
		(structurize-syntax
		  (cons no-uq@-constructor
		    (let loop ((result (reverse result)))
		      (if (null? result) '()
			(cons (let ((first (car result)))
				(if (qq-seq-entry-changed? first)
				  (qq-seq-entry-expanded first)
				  (list 'quote
				    (qq-seq-entry-orig first))))
			  (loop (cdr result))))))
		  expr)
		(structurize-syntax
		  (uq@-finalizer expr result
		    (let loop ((result (reverse result)))
		      (if (null? result)
			'()
			(let ((first (car result)))
			  (if (qq-seq-entry-changed? first)
			    (list
			      (if (eq? (qq-seq-entry-uq-type first) 'unquote)
				uq@-cons
				uq@-append)
			      (qq-seq-entry-expanded first)
			      (loop (cdr result)))
			    (list '#%cons
			      (list 'quote (qq-seq-entry-orig first))
			      (loop (cdr result))))))))
		  expr)))
	    expr)
	  (begin
	    (put-attribute attributes 'qq-changed? #f)
	    (let ((out (expand-expr (car items) env attributes vocab)))
	      (let ((this-changed? (get-attribute attributes 'qq-changed?))
		     (this-uq-type
		       (begin0
			 (get-attribute attributes 'qq-unquote-type)
			 (put-attribute attributes 'qq-unquote-type 'unquote))))
		(loop (cdr items)
		  (cons (make-qq-seq-entry (car items) out this-changed?
			  this-uq-type)
		    result)
		  (or changed? this-changed?)
		  (if this-changed?
		    (and no-uq-splicings?
		      (eq? this-uq-type 'unquote))
		    no-uq-splicings?))))))))))

(add-list-micro qq-vocab (common-sequence-handler expose-list
			   '#%list (lambda (source result x) x)
			   '#%cons '#%append))

(add-ilist-micro qq-vocab (common-sequence-handler expose-list
			    '#%list* (lambda (source result x)
				       (let ((first (car result)))
					 (if (and (qq-seq-entry-changed? first)
					       (eq? 'unquote-splicing
						 (qq-seq-entry-uq-type first)))
					   (static-error source
					     "Cannot use ,@ in last position")
					   `(#%apply #%list* ,x))))
			    '#%cons '#%append))

(add-lit-micro qq-vocab
  (lambda (expr env attributes vocab)
    (cond
      ((z:vector? expr)
	((common-sequence-handler z:read-object
	   '#%vector (lambda (source result x) (list '#%list->vector x))
	   '#%cons '#%append)
	  expr env attributes vocab))
      ((z:box? expr)
	((common-sequence-handler (lambda (e) (list (z:read-object e)))
	   '#%box (lambda (source result x)
		    (static-error source "Invalid use of ,@ inside box"))
	   '#%cons '#%append)
	  expr env attributes vocab))
      (else expr))))

(add-micro-form 'quasiquote qq-vocab
  (let* ((kwd '(quasiquote))
	  (in-pattern '(quasiquote template))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((template (pat:pexpand 'template p-env kwd))
		   (new-qq-level
		     (add1 (or (get-attribute attributes 'qq-level)
			     qq-base-level))))
	      (put-attribute attributes 'qq-level new-qq-level)
	      (let ((result (expand-expr template
			      env attributes qq-vocab)))
		(put-attribute attributes 'qq-level (sub1 new-qq-level))
		(structurize-syntax
		  (list '#%list ''quasiquote result)
		  expr)))))
	(else
	  (static-error expr "Malformed quasiquote"))))))

(add-micro-form 'quote qq-vocab
  (let* ((kwd '(quote))
	  (in-pattern '(quote body))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((body (pat:pexpand 'body p-env kwd)))
	      (structurize-syntax
		(list '#%list ''quote
		  (expand-expr body env attributes vocab))
		expr))))
	(else
	  (static-error expr "Malformed unquote"))))))

(define unquotation-handler
  (lambda (uq-type string-uq-type)
    (add-micro-form uq-type qq-vocab
      (let* ((kwd (list uq-type))
	      (in-pattern (list uq-type 'body))
	      (m&e (pat:make-match&env in-pattern kwd)))
	(lambda (expr env attributes vocab)
	  (cond
	    ((pat:match-against m&e expr env)
	      =>
	      (lambda (p-env)
		(let ((body (pat:pexpand 'body p-env kwd))
		       (qq-level (get-attribute attributes 'qq-level)))
		  (cond
		    ((= qq-level (add1 qq-base-level))
		      (put-attribute attributes 'qq-changed? #t)
		      (put-attribute attributes 'qq-unquote-type uq-type)
		      body)
		    ((>= qq-level qq-base-level)
		      (put-attribute attributes 'qq-level (sub1 qq-level))
		      (let ((result
			      (expand-expr body env attributes vocab)))
			(put-attribute attributes 'qq-level qq-level)
			(list '#%list `',uq-type result)))
		    (else
		      (static-error expr
			(string-append string-uq-type
			  " not inside quasiquote")))))))
	    (else
	      (static-error expr
		(string-append "Malformed " string-uq-type)))))))))

(unquotation-handler 'unquote "unquote")
(unquotation-handler 'unquote-splicing "unquote-splicing")
