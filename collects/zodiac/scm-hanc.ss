(define-struct signature-element ())
(define-struct (name-element struct:signature-element) (name))
(define-struct (unit-element struct:signature-element) (id signature))

(define immediate-signature-name '|<immediate signature>|)

(define cu/s-this-link-attr 'cu/s-this-link-name)

(define explode-signature-elements
  (lambda (elements)
    (map (lambda (elt)
	   (cond
	     ((name-element? elt)
	       (name-element-name elt))
	     ((unit-element? elt)
	       (cons (unit-element-id elt)
		 (signature-exploded (unit-element-signature elt))))
	     (else
	       (internal-error elt "Invalid signature element"))))
      elements)))

(define-struct signature (name elements exploded))

(define create-signature
  (opt-lambda (elements (name immediate-signature-name))
    (make-signature name elements
      (explode-signature-elements elements))))

(define add-signature
  (lambda (name attributes elements)
    (let ((sig-space (get-attribute attributes 'sig-space
		       (lambda ()
			 (let ((ss (make-hash-table)))
			   (put-attribute attributes 'sig-space ss)
			   ss)))))
      (hash-table-put! sig-space (z:read-object name)
	(create-signature elements (z:read-object name))))))

(define lookup-signature
  (lambda (name attributes)
    (let ((sig-space (get-attribute attributes 'sig-space)))
      (if sig-space
	(let ((entry
		(hash-table-get sig-space (z:read-object name)
		  (lambda () (static-error name "Unbound signature name")))))
	  entry)
	(static-error name "Unbound signature name")))))

(define extract-sub-unit-signature
  (lambda (signature indices)
    (if (null? indices)
      signature
      (let* ((first (car indices))
	      (raw-first (z:read-object first)))
	(let loop ((elements (signature-elements signature)))
	  (if (null? elements)
	    (static-error first "No such sub-unit in signature")
	    (if (unit-element? (car elements))
	      (if (eq? raw-first (unit-element-id (car elements)))
		(extract-sub-unit-signature
		  (unit-element-signature (car elements))
		  (cdr indices))
		(loop (cdr elements)))
	      (loop (cdr elements)))))))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(define cu/s-attr 'compound-unit/sig-table)

(define-struct tag-table-entry (signature))
(define-struct (tag-table-import-entry struct:tag-table-entry) ())
(define-struct (tag-table-link-entry struct:tag-table-entry) ())

(define extract-cu/s-tag-table
  (lambda (attributes)
    (car 
      (get-attribute attributes cu/s-attr
	(lambda ()
	  (internal-error "Unable to find compound-unit/sig attribute"))))))

(define cu/s-tag-table-put
  (lambda (maker)
    (lambda (table tag sig env attributes)
      (hash-table-put! table (z:read-object tag)
	(maker (expand-expr sig env attributes sig-vocab))))))

(define cu/s-tag-table-put/import
  (cu/s-tag-table-put make-tag-table-import-entry))

(define cu/s-tag-table-put/link
  (cu/s-tag-table-put make-tag-table-link-entry))

(define cu/s-tag-table-lookup
  (opt-lambda (table tag (not-found (lambda () #f)))
    (hash-table-get table (z:read-object tag) not-found)))

(define cu/s-tag-table-lookup/static-error
  (lambda (table tag)
    (cu/s-tag-table-lookup table tag
      (lambda ()
	(static-error tag "Unbound tag")))))

(define cu/s-tag-table-lookup/internal-error
  (lambda (table tag)
    (cu/s-tag-table-lookup table tag
      (lambda ()
	(internal-error tag "Should have been bound")))))

; --------------------------------------------------------------------

(define sig-vocab (make-vocabulary))

(add-sym-micro sig-vocab
  (lambda (expr env attributes vocab)
    (lookup-signature expr attributes)))

(add-list-micro sig-vocab
  (lambda (expr env attributes vocab)
    (let ((contents (expose-list expr)))
      (create-signature
	(apply append
	  (map (lambda (e)
		 (expand-expr e env attributes sig-element-vocab))
	    contents))))))

; --------------------------------------------------------------------

(define sig-element-vocab (make-vocabulary))

(add-sym-micro sig-element-vocab
  (lambda (expr env attributes vocab)
    (list (make-name-element (z:read-object expr)))))

(add-micro-form 'struct sig-element-vocab
  (let* ((kwd '(struct))
	  (in-pattern '(struct base (field ...) omit ...))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((base (pat:pexpand 'base p-env kwd))
		   (fields (pat:pexpand '(field ...) p-env kwd))
		   (omits (pat:pexpand '(omit ...) p-env kwd)))
	      (valid-syntactic-id? base)
	      (valid-syntactic-id/s? fields)
	      (unless (null? omits)
		(internal-error expr
		  "Omission specifications not yet handled"))
	      (map make-name-element
		(map z:read-object
		  (generate-struct-names base fields expr))))))
	(else
	  (static-error expr "Malformed struct clause"))))))

(add-micro-form 'open sig-element-vocab
  (let* ((kwd '(open))
	  (in-pattern '(open sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? sig)
	      (expand-expr sig env attributes sig-vocab))))
	(else
	  (static-error expr "Malformed open clause"))))))

(add-micro-form 'unit sig-element-vocab
  (let* ((kwd '(unit :))
	  (in-pattern '(unit id : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (list (make-unit-element (z:read-object id)
		      (expand-expr sig env attributes sig-vocab))))))
	(else
	  (static-error expr "Malformed unit clause"))))))

; --------------------------------------------------------------------

(define u/s-prim-imports-vocab (make-vocabulary))

(add-sym-micro u/s-prim-imports-vocab
  (lambda (expr env attributes vocab)
    (convert-to-prim-format
      (signature-elements
	(lookup-signature expr attributes)))))

(add-list-micro u/s-prim-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(id : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (convert-to-prim-format
		(signature-elements
		  (expand-expr sig env attributes sig-vocab))
		(z:read-object id)))))
	(else
	  (convert-to-prim-format
	    (apply append
	      (map (lambda (s)
		     (expand-expr s env attributes sig-element-vocab))
		(expose-list expr)))))))))

(define convert-to-prim-format
  (opt-lambda (sig-elements (prefix-symbol #f))
    (convert-to-prim-format-helper sig-elements
      (if prefix-symbol
	(string-append (symbol->string prefix-symbol) ":")
	""))))

(define convert-to-prim-format-helper
  (lambda (sig-elements prefix-string)
    (apply append
      (map (lambda (elt)
	     (cond
	       ((name-element? elt)
		 (list
		   (string->symbol
		     (string-append prefix-string
		       (symbol->string (name-element-name elt))))))
	       ((unit-element? elt)
		 (let ((new-prefix
			 (string-append prefix-string
			   (symbol->string (unit-element-id elt))
			   ":")))
		   (convert-to-prim-format-helper
		     (signature-elements
		       (unit-element-signature elt))
		     new-prefix)))
	       (else
		 (internal-error elt "Illegal signature element"))))
	sig-elements))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(define u/s-sign-imports-vocab (make-vocabulary))

(add-sym-micro u/s-sign-imports-vocab
  (lambda (expr env attributes vocab)
    (cons (z:read-object expr)
      (signature-exploded
	(lookup-signature expr attributes)))))

(add-list-micro u/s-sign-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(id : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (cons (z:read-object id)
		(signature-exploded
		  (lookup-signature sig attributes))))))
	(else
	  (cons immediate-signature-name
	    (explode-signature-elements
	      (apply append
		(map (lambda (s)
		       (expand-expr s env attributes sig-element-vocab))
		  (expose-list expr))))))))))

; --------------------------------------------------------------------

(define create-prim-exports
  (lambda (export-sig renames source env attributes)
    (let ((sig-names (signature-elements
		       (expand-expr export-sig env attributes sig-vocab))))
      (let ((table (make-hash-table)))
	(for-each (lambda (z-rename)
		    (let ((rename-couple (expose-list z-rename)))
		      (hash-table-put! table
			(z:read-object (cadr rename-couple))
			(z:read-object (car rename-couple)))))
	  renames)
	(let loop ((sig-names sig-names))
	  (if (null? sig-names)
	    '()
	    (let ((first (car sig-names)))
	      (when (unit-element? first)
		(static-error source "Unit exports not allowed"))
	      (let ((name (name-element-name first)))
		(cons
		  (let ((entry (hash-table-get table name (lambda () #f))))
		    (if entry
		      (list entry name)
		      (list name name)))
		  (loop (cdr sig-names)))))))))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(define u/s-sign-exports-vocab (make-vocabulary))

(add-sym-micro u/s-sign-exports-vocab
  (lambda (expr env attributes vocab)
    (cons (z:read-object expr)
      (signature-exploded
	(lookup-signature expr attributes)))))

(add-list-micro u/s-sign-exports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(id : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (cons (z:read-object id)
		(signature-exploded
		  (lookup-signature sig attributes))))))
	(else
	  (cons immediate-signature-name
	    (explode-signature-elements
	      (apply append
		(map (lambda (s)
		       (expand-expr s env attributes sig-element-vocab))
		  (expose-list expr))))))))))

; --------------------------------------------------------------------

(add-micro-form 'define-signature scheme-vocabulary
  (let* ((kwd '(define-signature))
	  (in-pattern '(define-signature name sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((name (pat:pexpand 'name p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? name)
	      (let ((elements
		      (signature-elements
			(expand-expr sig env attributes sig-vocab))))
		(add-signature name attributes elements))
	      (expand-expr
		(structurize-syntax '(#%void) expr)
		env attributes vocab))))
	(else
	  (static-error expr "Malformed define-signature"))))))

(add-micro-form 'unit/sig scheme-vocabulary
  (let* ((kwd-1 '(unit/sig import rename))
	  (in-pattern-1 '(unit/sig signature
			   (import imports ...)
			   (rename renames ...)
			   clauses ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	  (kwd-2 '(unit/sig import))
	  (in-pattern-2 '(unit/sig signature
			   (import imports ...)
			   clauses ...))
	  (out-pattern-2 '(unit/sig signature
			    (import imports ...)
			    (rename)
			    clauses ...))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd-2)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let* ((in:signature (pat:pexpand 'signature p-env kwd-1))
		    (in:imports (pat:pexpand '(imports ...) p-env kwd-1))
		    (in:renames (pat:pexpand '(renames ...) p-env kwd-1))
		    (in:clauses (pat:pexpand '(clauses ...) p-env kwd-1)))
	      (let* ((prim-unit:imports (apply append
					  (map (lambda (import)
						 (expand-expr import env
						   attributes
						   u/s-prim-imports-vocab))
					    in:imports)))
		      (prim-unit:exports (create-prim-exports in:signature
					   in:renames expr env attributes))
		      (prim-unit:clauses in:clauses)
		      (sign-unit:imports (map (lambda (import)
						(expand-expr import env
						  attributes
						  u/s-sign-imports-vocab))
					   in:imports))
		      (sign-unit:exports (expand-expr in:signature env
					   attributes u/s-sign-exports-vocab)))
		(expand-expr
		  (structurize-syntax
		    `(#%make-unit-with-signature
		       (unit
			 (import ,@prim-unit:imports)
			 (export ,@prim-unit:exports)
			 ,@prim-unit:clauses)
		       (quote ,sign-unit:imports)
		       (quote ,sign-unit:exports))
		    expr)
		  env attributes vocab)))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (expand-expr
	      (structurize-syntax
		(pat:pexpand out-pattern-2 p-env kwd-2)
		expr)
	      env attributes vocab)))
	(else
	  (static-error expr "Malformed unit/sig"))))))

; --------------------------------------------------------------------

(define cu/s-imports-record-tag-sigs-vocab (make-vocabulary))

(add-list-micro cu/s-imports-record-tag-sigs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (let ((table (extract-cu/s-tag-table attributes)))
		(when (cu/s-tag-table-lookup table tag)
		  (static-error tag
		    "Duplicate tag definition"))
		(cu/s-tag-table-put/import table tag sig env attributes)))))
	(else
	  (static-error expr "Malformed compound-unit/sig import clause"))))))

(define cu/s-sign-imports-vocab (make-vocabulary))

(add-list-micro cu/s-sign-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (let ((table (extract-cu/s-tag-table attributes)))
		(signature-exploded
		  (tag-table-entry-signature
		    (cu/s-tag-table-lookup/internal-error table tag)))))))
	(else
	  (static-error expr "Malformed compound-unit/sig import clause"))))))

(define cu/s-link-imports-vocab (make-vocabulary))

(add-list-micro cu/s-link-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (let ((table (extract-cu/s-tag-table attributes)))
		(convert-to-prim-format
		  (signature-elements
		    (tag-table-entry-signature
		      (cu/s-tag-table-lookup/internal-error table tag))))))))
	(else
	  (static-error expr "Malformed compound-unit/sig import clause"))))))

; --------------------------------------------------------------------

(define cu/s-link-record-tag-sigs-vocab (make-vocabulary))

(add-list-micro cu/s-link-record-tag-sigs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig misc))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (let ((table (extract-cu/s-tag-table attributes)))
		(when (cu/s-tag-table-lookup table tag)
		  (static-error tag
		    "Duplicate tag definition"))
		(cu/s-tag-table-put/link table tag sig env attributes)))))
	(else
	  (static-error expr "Malformed compound-unit/sig link clause"))))))

(define cu/s-link-exports-vocab (make-vocabulary))

(add-list-micro cu/s-link-exports-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig misc))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (let ((table (extract-cu/s-tag-table attributes)))
		(signature-exploded
		  (tag-table-entry-signature
		    (cu/s-tag-table-lookup/internal-error table tag)))))))
	(else
	  (static-error expr "Malformed compound-unit/sig link clause"))))))

(define cu/s-link-tags-vocab (make-vocabulary))

(add-list-micro cu/s-link-tags-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig misc))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      tag)))
	(else
	  (static-error expr "Malformed compound-unit/sig link clause"))))))

(define cu/s-link-exprs-vocab (make-vocabulary))

(add-list-micro cu/s-link-exprs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig (expr path ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((expr (pat:pexpand 'expr p-env kwd)))
	      expr)))
	(else
	  (static-error expr "Malformed compound-unit/sig link clause"))))))

(define cu/s-link-linking-sigs-vocab (make-vocabulary))

(add-list-micro cu/s-link-linking-sigs-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig (expr path ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (path-elts (pat:pexpand '(path ...) p-env kwd)))
	      (map (lambda (p)
		     ; NOTE: use this to check for self-imports.
		     (put-attribute attributes cu/s-this-link-attr
		       (z:read-object tag))
		     (expand-expr p env attributes
		       cu/s-unit-path-linkage-vocab))
		path-elts))))
	(else
	  (static-error expr "Malformed compound-unit/sig link clause"))))))

(define cu/s-link-prim-unit-names-vocab (make-vocabulary))

(add-list-micro cu/s-link-prim-unit-names-vocab
  (let* ((kwd '(:))
	  (in-pattern '(tag : sig (expr path ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (path-elts (pat:pexpand '(path ...) p-env kwd)))
	      (apply append
		(map (lambda (p)
		       (expand-expr p env attributes
			 cu/s-unit-path-prim-links-vocab))
		  path-elts)))))
	(else
	  (static-error expr "Malformed compound-unit/sig link clause"))))))

; --------------------------------------------------------------------

(define cu/s-unit-path-extract-final-sig-vocab (make-vocabulary))

(add-sym-micro cu/s-unit-path-extract-final-sig-vocab
  (lambda (expr env attributes vocab)
    (let ((sig
	    (tag-table-entry-signature
	      (cu/s-tag-table-lookup/static-error
		(extract-cu/s-tag-table attributes)
		expr))))
      sig)))

(add-list-micro cu/s-unit-path-extract-final-sig-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(tag : sig))
	  (in-pattern-2 '(tag id ...))
	  (in-pattern-3 '((tag id ...) : sig))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (let ((big-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag)))
		     (small-sig
		       (expand-expr sig env attributes sig-vocab)))
		(verify-signature-match 'compound-unit/sig
		  #f
		  (format "signature ~s" (signature-name small-sig))
		  (signature-exploded small-sig)
		  (format "signature ~s" (signature-name big-sig))
		  (signature-exploded big-sig))
		small-sig))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (valid-syntactic-id? tag)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids)))
		  final-sig)))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids))
		       (small-sig
			 (expand-expr sig env attributes tag)))
		  (verify-signature-match 'compound-unit/sig
		    #f
		    (format "signature ~s" (signature-name small-sig))
		    (signature-exploded small-sig)
		    (format "signature ~s" (signature-name final-sig))
		    (signature-exploded final-sig))
		  small-sig)))))
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-unit-path-linkage-vocab (make-vocabulary))

(add-sym-micro cu/s-unit-path-linkage-vocab
  (lambda (expr env attributes vocab)
    (let ((sig
	    (tag-table-entry-signature
	      (cu/s-tag-table-lookup/static-error
		(extract-cu/s-tag-table attributes)
		expr))))
      (cons (z:read-object expr)
	(signature-exploded sig)))))

(add-list-micro cu/s-unit-path-linkage-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(tag : sig))
	  (in-pattern-2 '(tag id ...))
	  (in-pattern-3 '((tag id ...) : sig))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (let ((big-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag)))
		     (small-sig
		       (expand-expr sig env attributes sig-vocab)))
		(verify-signature-match 'compound-unit/sig
		  #f
		  (format "signature ~s" (signature-name small-sig))
		  (signature-exploded small-sig)
		  (format "signature ~s" (signature-name big-sig))
		  (signature-exploded big-sig))
		(cons (z:read-object tag)
		  (signature-exploded small-sig))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (valid-syntactic-id? tag)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids)))
		  (cons (z:read-object tag)
		    (signature-exploded final-sig)))))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids))
		       (small-sig
			 (expand-expr sig env attributes tag)))
		  (verify-signature-match 'compound-unit/sig
		    #f
		    (format "signature ~s" (signature-name small-sig))
		    (signature-exploded small-sig)
		    (format "signature ~s" (signature-name final-sig))
		    (signature-exploded final-sig))
		  (cons (z:read-object tag)
		    (signature-exploded small-sig)))))))
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-unit-path-prim-links-vocab (make-vocabulary))

(add-sym-micro cu/s-unit-path-prim-links-vocab
  (lambda (expr env attributes vocab)
    (let ((tag-table-entry
	    (cu/s-tag-table-lookup/static-error
	      (extract-cu/s-tag-table attributes)
	      expr)))
      (let ((sig (tag-table-entry-signature tag-table-entry)))
	(cond
	  ((tag-table-import-entry? tag-table-entry)
	    (cu/s-build-link-names sig
	      (string-append
		(cu/s-build-link-prefix (list expr))
		":")))
	  ((tag-table-link-entry? tag-table-entry)
	    (list
	      (cons (z:read-object expr)
		(cu/s-build-link-names sig))))
	  (else
	    (internal-error tag-table-entry "Illegal tag-table entry")))))))

(add-list-micro cu/s-unit-path-prim-links-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(tag : sig))
	  (in-pattern-2 '(tag id ...))
	  (in-pattern-3 '((tag id ...) : sig))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (let ((small-sig
		       (expand-expr sig env attributes sig-vocab)))
		(let ((tag-table-entry (cu/s-tag-table-lookup/internal-error
					 (extract-cu/s-tag-table attributes)
					 tag)))
		  (cond
		    ((tag-table-import-entry? tag-table-entry)
		      (cu/s-build-link-names small-sig
			(string-append
			  (cu/s-build-link-prefix (list tag))
			  ":")))
		    ((tag-table-link-entry? tag-table-entry)
		      (list
			(cons (z:read-object tag)
			  (cu/s-build-link-names small-sig))))
		    (else
		      (internal-error tag-table-entry
			"Illegal tag-table entry"))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids)))
		  (let ((tag-table-entry (cu/s-tag-table-lookup/internal-error
					   (extract-cu/s-tag-table attributes)
					   tag)))
		    (cond
		      ((tag-table-import-entry? tag-table-entry)
			(cu/s-build-link-names final-sig
			  (cu/s-build-link-prefix ids)))
		      ((tag-table-link-entry? tag-table-entry)
			(list
			  (cons (z:read-object tag)
			    (cu/s-build-link-names final-sig
			      (cu/s-build-link-prefix ids)))))
		      (else
			(internal-error tag-table-entry
			  "Illegal tag-table entry")))))))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (let ((small-sig
		      (expand-expr sig env attributes tag)))
		(let ((tag-table-entry (cu/s-tag-table-lookup/internal-error
					 (extract-cu/s-tag-table attributes)
					 tag)))
		  (cond
		    ((tag-table-import-entry? tag-table-entry)
		      (cu/s-build-link-names small-sig
			(cu/s-build-link-prefix ids)))
		    ((tag-table-link-entry? tag-table-entry)
		      (list
			(cons (z:read-object tag)
			  (cu/s-build-link-names small-sig
			    (cu/s-build-link-prefix ids)))))
		    (else
		      (internal-error tag-table-entry
			"Illegal tag-table entry"))))))))
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-build-link-names
  (opt-lambda (signature (prefix-string ""))
    (convert-to-prim-format-helper (signature-elements signature)
      prefix-string)))

(define cu/s-build-link-prefix
  (lambda (ids)
    (if (null? ids)
      ""
      (apply string-append
	(let loop ((str-ids (map symbol->string
			      (map z:read-object ids))))
	  (if (null? (cdr str-ids))
	    (list (car str-ids))
	    (cons (car str-ids)
	      (cons ":"
		(loop (cdr str-ids))))))))))

; --------------------------------------------------------------------

(define-struct cu/s-export ())
(define-struct (cu/s-var-export struct:cu/s-export) (var external))
(define-struct (cu/s-unit-export struct:cu/s-export) (sig name))
(define-struct (cu/s-open-export struct:cu/s-export) (sig))

(define cu/s-verify-variable-in-path
  (lambda (path variable env attributes)
    (let ((tag-table (extract-cu/s-tag-table attributes)))
      (let ((final-sig (expand-expr path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
	(cu/s-verify-variable-in-sig
	  (signature-exploded final-sig)
	  variable)))))

(define cu/s-verify-variable-in-sig
  (lambda (sig variable)
    (let ((raw-var (z:read-object variable)))
      (let loop ((elements (signature-elements sig)))
	(if (null? elements)
	  (static-error variable "No such variable in signature")
	  (or (and (name-element? (car elements))
		(eq? raw-var (name-element-name (car elements))))
	    (loop (cdr elements))))))))

(define cu/s-export-sign-vocab (make-vocabulary))

(add-micro-form 'var cu/s-export-sign-vocab
  (let* ((kwd '(var))
	  (in-pattern-1 '(var (unit-path variable)))
	  (in-pattern-2 '(var (unit-path variable) external-variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (cu/s-verify-variable-in-path unit-path variable
		env attributes)
	      (make-cu/s-var-export (z:read-object variable)
		(z:read-object variable)))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd))
		   (external (pat:pexpand 'external-variable p-env kwd)))
	      (cu/s-verify-variable-in-path unit-path variable
		env attributes)
	      (make-cu/s-var-export (z:read-object variable)
		(z:read-object external)))))
	(else
	  (static-error expr "Malformed var export"))))))

(add-micro-form 'open cu/s-export-sign-vocab
  (let* ((kwd '(open))
	  (in-pattern '(open unit-path))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((final-sig
		      (expand-expr unit-path env attributes
			cu/s-unit-path-extract-final-sig-vocab)))
		(make-cu/s-open-export final-sig)))))
	(else
	  (static-error expr "Malformed open export"))))))

(add-micro-form 'unit cu/s-export-sign-vocab
  (let* ((kwd '(unit))
	  (in-pattern-1 '(unit (unit-path variable)))
	  (in-pattern-2 '(unit (unit-path variable) external-variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((final-sig
		      (expand-expr unit-path env attributes
			cu/s-unit-path-extract-final-sig-vocab)))
		(make-cu/s-unit-export final-sig
		  (extract-final-name unit-path))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (make-cu/s-unit-export final-sig
		(z:read-object variable)))))
	(else
	  (static-error expr "Malformed unit export"))))))

; --------------------------------------------------------------------

(define record-tag-signatures
  (lambda (imports links env attributes)
    (map (lambda (i)
	   (expand-expr i env attributes cu/s-imports-record-tag-sigs-vocab))
      imports)
    (map (lambda (l)
	   (expand-expr l env attributes cu/s-link-record-tag-sigs-vocab))
      links)))

(add-micro-form 'compound-unit/sig scheme-vocabulary
  (let* ((kwd '(compound-unit/sig import link export))
	  (in-pattern '(compound-unit/sig
			 (import imports ...)
			 (link links ...)
			 (export exports ...)))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (put-attribute attributes cu/s-attr
	      (cons (make-hash-table)
		(get-attribute attributes cu/s-attr
		  (lambda () '()))))
	    (let* ((in:imports (pat:pexpand '(imports ...) p-env kwd))
		    (in:links (pat:pexpand '(links ...) p-env kwd))
		    (in:exports (pat:pexpand '(exports ...) p-env kwd)))
	      (record-tag-signatures in:imports in:links env attributes)
	      ; linkage = given to verify-linkage-signature-match
	      ; prim = goes into underlying compound-unit
	      ; sign = given to make-unit-with-signature
	      (let* ((linkage:tags (map (lambda (l)
					  (expand-expr l env attributes
					    cu/s-link-tags-vocab))
				     in:links))
		      (linkage:unit-vars linkage:tags)
		      (linkage:unit-exprs (map (lambda (l)
						 (expand-expr l env attributes
						   cu/s-link-exprs-vocab))
					    in:links))
		      (linkage:link-exports
			(map (lambda (l)
			       (expand-expr l env attributes
				 cu/s-link-exports-vocab))
			  in:links))
		      (linkage:link-imports
			(map (lambda (l)
			       (expand-expr l env attributes
				 cu/s-link-linking-sigs-vocab))
			  in:links))
		      (prim:imports (apply append
				      (map (lambda (l)
					     (expand-expr l env attributes
					       cu/s-link-imports-vocab))
					in:imports)))
		      (prim:links (map (lambda (l)
					 (expand-expr l env attributes
					   cu/s-link-prim-unit-names-vocab))
				    in:links))
		      (prim:exports 1729)
		      (sign:imports (map (lambda (i)
					   (expand-expr i env attributes
					     cu/s-sign-imports-vocab))
				      in:imports))
		      (sign:exports 1729))
		(newline)
		(expand-expr
		  (structurize-syntax
		    `(let ,(map list linkage:unit-vars linkage:unit-exprs)
		       (#%verify-linkage-signature-match
			 'compound-unit/sig
			 ',linkage:tags
			 (#%list ,@linkage:unit-vars)
			 ',linkage:link-exports
			 ',linkage:link-imports))
		    expr)
		  env attributes vocab)))))
	(else
	  (static-error expr "Malformed compound-unit/sig"))))))
