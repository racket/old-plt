(define-struct signature-element (source))
(define-struct (name-element struct:signature-element) (name))
(define-struct (unit-element struct:signature-element) (id signature))

(define immediate-signature-name '|<immediate signature>|)

(define cu/s-this-link-attr 'cu/s-this-link-name)

(define-struct signature (name elements exploded))

; This is based on code lifted from Matthew's implementation (note the
; use of brackets (-:).

(define verify-duplicates-&-sort-signature-elements
  (lambda (elements)
    (let loop ((seen '()) (rest elements))
      (unless (null? rest)
	(let ((first (car rest)))
	  (let ((first-name
		  (cond
		    ((name-element? first)
		      (name-element-name first))
		    ((unit-element? first)
		      (unit-element-id first))
		    (else
		      (internal-error first "Invalid unit element")))))
	    (when (memq first-name seen)
	      (static-error (signature-element-source first)
		"Duplicate signature entry: ~s" first-name))
	    (loop (cons first-name seen) (cdr rest))))))
    (letrec
      ((split
	 (lambda (l f s)
	   (cond
	     [(null? l) (values f s)]
	     [(null? (cdr l)) (values (cons (car l) f) s)]
	     [else (split (cddr l) (cons (car l) f)
		     (cons (cadr l) s))])))
	(merge
	  (lambda (f s)
	    (cond
	      [(null? f) s]
	      [(null? s) f]
	      [(less-than? (car s) (car f))
		(cons (car s) (merge f (cdr s)))]
	      [else
		(cons (car f) (merge (cdr f) s))])))
	(less-than?
	  (lambda (a b)
	    (if (name-element? a)
	      (if (name-element? b)
		(symbol-less-than? (name-element-name a)
		  (name-element-name b))
		#t)
	      (if (name-element? b)
		#f
		(symbol-less-than? (unit-element-id a)
		  (unit-element-id b))))))
	(symbol-less-than?
	  (lambda (a b)
	    (string<? (symbol->string a) (symbol->string b)))))
      (let loop ([elements elements])
	(cond
	  [(null? elements) null]
	  [(null? (cdr elements)) elements]
	  [else (let-values ([(f s) (split elements null null)])
		  (merge (loop f) (loop s)))])))))

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

(define create-signature
  (opt-lambda (elements (name immediate-signature-name))
    (let ((sorted-elements
	    (verify-duplicates-&-sort-signature-elements elements)))
      (make-signature name sorted-elements
	(explode-signature-elements sorted-elements)))))

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
	  (internal-error attributes
	    "Unable to find compound-unit/sig attribute"))))))

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

(define sig-vocab (create-vocabulary 'sig-vocab))

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

(define sig-element-vocab (create-vocabulary 'sig-element-vocab))

(add-sym-micro sig-element-vocab
  (lambda (expr env attributes vocab)
    (list (make-name-element expr (z:read-object expr)))))

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
		   (in:omits (pat:pexpand '(omit ...) p-env kwd)))
	      (valid-syntactic-id? base)
	      (valid-syntactic-id/s? fields)
	      (let ((omit-names
		      (map (lambda (o)
			     (expand-expr o env attributes
			       signature-struct-omission-checker-vocab))
			in:omits)))
		(let ((generated-names
			(map z:read-object
			  (generate-struct-names base fields expr
			    (memq '-selectors omit-names)
			    (memq '-setters omit-names)))))
		  (let loop ((omits omit-names))
		    (unless (null? omits)
		      (let ((first (car omits)))
			(when (z:symbol? first)
			  (unless (memq (z:read-object first) generated-names)
			    (static-error first
			      "Name not generated; illegal to omit")))
			(loop (cdr omits)))))
		  (let ((real-omits
			  (let loop ((omits omit-names))
			    (if (null? omits) '()
			      (if (symbol? (car omits))
				(loop (cdr omits))
				(cons (z:read-object (car omits))
				  (loop (cdr omits))))))))
		    (let loop ((names generated-names))
		      (if (null? names) '()
			(if (memq (car names) real-omits)
			  (loop (cdr names))
			  (cons (make-name-element expr (car names))
			    (loop (cdr names))))))))))))
	(else
	  (static-error expr "Malformed struct clause"))))))

(define signature-struct-omission-checker-vocab
  (create-vocabulary 'signature-struct-omission-checker-vocab))

(add-sym-micro signature-struct-omission-checker-vocab
  (lambda (expr env attributes vocab)
    (let ((raw-expr (z:read-object expr)))
      (unless (memq raw-expr '(-selectors -setters))
	(static-error expr "Invalid omission specifier"))
      raw-expr)))

(add-micro-form '- signature-struct-omission-checker-vocab
  (let* ((kwd '(-))
	  (in-pattern '(- var))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((var (pat:pexpand 'var p-env kwd)))
	      (valid-syntactic-id? var)
	      (structurize-syntax (z:read-object var) expr))))
	(else
	  (static-error expr "Malformed omission specifier"))))))

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
	      (signature-elements
		(expand-expr sig env attributes sig-vocab)))))
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
	      (list (make-unit-element expr (z:read-object id)
		      (expand-expr sig env attributes sig-vocab))))))
	(else
	  (static-error expr "Malformed unit clause"))))))

; --------------------------------------------------------------------

(define u/s-prim-imports-vocab
  (create-vocabulary 'u/s-prim-imports-vocab))

(add-sym-micro u/s-prim-imports-vocab
  (lambda (expr env attributes vocab)
    (convert-to-prim-format
      (signature-elements
	(lookup-signature expr attributes)))))

(add-list-micro u/s-prim-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (convert-to-prim-format
		(signature-elements
		  (expand-expr sig env attributes sig-vocab))
		(z:read-object id)))))
	((pat:match-against m&e-2 expr env)
	  (static-error expr "Ambiguous : in signature"))
	(else
	  (convert-to-prim-format
	    (signature-elements
	      (expand-expr expr env attributes sig-vocab))))))))

(define convert-to-prim-format
  (opt-lambda (sig-elements (prefix #f))
    (convert-to-prim-format-helper sig-elements
      (cond
	((symbol? prefix)
	  (let ((s (symbol->string prefix)))
	    (if (string=? "" s)
	      s
	      (string-append s ":"))))
	((string? prefix)
	  prefix)
	(else
	  "")))))

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

(define u/s-sign-imports-vocab
  (create-vocabulary 'u/s-sign-imports-vocab))

(add-sym-micro u/s-sign-imports-vocab
  (lambda (expr env attributes vocab)
    (cons (z:read-object expr)
      (signature-exploded
	(lookup-signature expr attributes)))))

(add-list-micro u/s-sign-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (cons (z:read-object id)
		(signature-exploded
		  (expand-expr sig env attributes sig-vocab))))))
	((pat:match-against m&e-2 expr env)
	  (static-error expr "Ambiguous : in signature"))
	(else
	  (cons immediate-signature-name
	    (explode-signature-elements
	      (signature-elements
		(expand-expr expr env attributes sig-vocab)))))))))

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

(define u/s-sign-exports-vocab
  (create-vocabulary 'u/s-sign-exports-vocab))

(add-sym-micro u/s-sign-exports-vocab
  (lambda (expr env attributes vocab)
    (signature-exploded
      (lookup-signature expr attributes))))

(add-list-micro u/s-sign-exports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((id (pat:pexpand 'id p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? id)
	      (signature-exploded
		(expand-expr sig env attributes sig-vocab)))))
	((pat:match-against m&e-2 expr env)
	  (static-error expr "Ambiguous : in signature"))
	(else
	  (explode-signature-elements
	    (signature-elements
	      (expand-expr expr env attributes sig-vocab))))))))

; --------------------------------------------------------------------

(add-primitivized-micro-form 'define-signature scheme-vocabulary
  (let* ((kwd '())
	  (in-pattern '(_ name sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((name (pat:pexpand 'name p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? name)
	      (unless (get-top-level-status attributes)
		  (static-error expr "Only supported at top-level"))
	      (let ((elements
		      (signature-elements
			(expand-expr sig env attributes sig-vocab))))
		(add-signature name attributes elements))
	      (expand-expr
		(structurize-syntax '(#%void) expr)
		env attributes vocab))))
	(else
	  (static-error expr "Malformed define-signature"))))))

(define u/s-expand-includes-vocab (create-vocabulary 'u/s-expand-includes-vocab))

(add-list-micro u/s-expand-includes-vocab
  (lambda (expr env attributes vocab)
    (list expr)))

(add-ilist-micro u/s-expand-includes-vocab
  (lambda (expr env attributes vocab)
    (list expr)))

(add-sym-micro u/s-expand-includes-vocab
  (lambda (expr env attributes vocab)
    (list expr)))

(add-lit-micro u/s-expand-includes-vocab
  (lambda (expr env attributes vocab)
    (list expr)))

(add-primitivized-micro-form 'include u/s-expand-includes-vocab
  (let* ((kwd '())
          (in-pattern '(_ filename))
          (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
        ((pat:match-against m&e expr env)
          =>
          (lambda (p-env)
            (let ((filename (pat:pexpand 'filename p-env kwd)))
              (unless (z:string? filename)
                (static-error filename "File name must be a string"))
              (let ((raw-filename (z:read-object filename)))
                (let-values (((base name dir?) (split-path raw-filename)))
                  (when dir?
                    (static-error filename "Cannot include a directory"))
                  (let ((original-directory (current-directory))
                         (p (with-handlers
			      ((exn:i/o:filesystem:filename?
				 (lambda (exn)
				   (static-error filename
				     "Unable to open file ~s" raw-filename))))
			      (open-input-file raw-filename))))
                    (dynamic-wind
                      (lambda ()
                        (when (string? base)
                          (current-directory base)))
                      (lambda ()
                        (apply append
                          (map (lambda (e)
                                 (expand-expr e env attributes
                                   vocab))
                            (let ((reader (z:read p
                                            (z:make-location
                                              (z:location-line
                                                z:default-initial-location)
                                              (z:location-column
                                                z:default-initial-location)
                                              (z:location-offset
                                                z:default-initial-location)
                                              (build-path (current-directory)
                                                name)))))
                              (let loop ()
                                (let ((input (reader)))
                                  (if (z:eof? input)
                                    '()
                                    (cons input
                                      (loop)))))))))
                      (lambda ()
                        (current-directory original-directory)
                        (close-input-port p)))))))))
        (else
          (static-error expr "Malformed include"))))))

(add-primitivized-micro-form 'unit/sig scheme-vocabulary
  (let* ((kwd-1 '(import rename))
	  (in-pattern-1 '(_ signature
			   (import imports ...)
			   (rename renames ...)
			   clauses ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd-1))
	  (kwd-2 '(import))
	  (in-pattern-2 '(_ signature
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
		      (prim-unit:clauses
			(apply append
			  (map (lambda (clause)
				 (expand-expr clause env attributes
				   u/s-expand-includes-vocab))
			    in:clauses)))
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

(define cu/s-imports-record-tag-sigs-vocab
  (create-vocabulary 'cu/s-imports-record-tag-sigs-vocab))

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

(define cu/s-sign-imports-vocab
  (create-vocabulary 'cu/s-sign-imports-vocab))

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
		(cons (z:read-object tag)
		  (signature-exploded
		    (tag-table-entry-signature
		      (cu/s-tag-table-lookup/internal-error table tag))))))))
	(else
	  (static-error expr "Malformed compound-unit/sig import clause"))))))

(define cu/s-link-imports-vocab
  (create-vocabulary 'cu/s-link-imports-vocab))

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
		      (cu/s-tag-table-lookup/internal-error table tag)))
		  (z:read-object tag))))))
	(else
	  (static-error expr "Malformed compound-unit/sig import clause"))))))

; --------------------------------------------------------------------

(define cu/s-link-record-tag-sigs-vocab
  (create-vocabulary 'cu/s-link-record-tag-sigs-vocab))

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

(define cu/s-link-exports-vocab
  (create-vocabulary 'cu/s-link-exports-vocab))

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

(define cu/s-link-tags-vocab
  (create-vocabulary 'cu/s-link-tags-vocab))

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

(define cu/s-link-exprs-vocab
  (create-vocabulary 'cu/s-link-exprs-vocab))

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

(define cu/s-link-linking-sigs-vocab
  (create-vocabulary 'cu/s-link-linking-sigs-vocab))

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
		     (put-attribute attributes cu/s-this-link-attr
		       (z:read-object tag))
		     (expand-expr p env attributes
		       cu/s-unit-path-linkage-vocab))
		path-elts))))
	(else
	  (static-error expr "Malformed compound-unit/sig link clause"))))))

(define cu/s-check-self-import
  (lambda (tag attributes)
    (when (eq? (z:read-object tag)
	    (get-attribute attributes cu/s-this-link-attr
	      (lambda () (internal-error tag "No this-link attribute"))))
      (static-error tag "Self import of tag ~s" (z:read-object tag)))))

(define cu/s-link-prim-unit-names-vocab
  (create-vocabulary 'cu/s-link-prim-unit-names-vocab))

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

(define cu/s-unit-path-extract-final-sig-vocab
  (create-vocabulary 'cu/s-unit-path-extract-final-sig-vocab))

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
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
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
			 (expand-expr sig env attributes sig-vocab)))
		  (verify-signature-match 'compound-unit/sig
		    #f
		    (format "signature ~s" (signature-name small-sig))
		    (signature-exploded small-sig)
		    (format "signature ~s" (signature-name final-sig))
		    (signature-exploded final-sig))
		  small-sig)))))
	((pat:match-against m&e-2 expr env)
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
	((pat:match-against m&e-3 expr env)
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
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-unit-path-linkage-vocab
  (create-vocabulary 'cu/s-unit-path-linkage-vocab))

(add-sym-micro cu/s-unit-path-linkage-vocab
  (lambda (expr env attributes vocab)
    (cu/s-check-self-import expr attributes)
    (let ((sig
	    (tag-table-entry-signature
	      (cu/s-tag-table-lookup/static-error
		(extract-cu/s-tag-table attributes)
		expr))))
      (cons (z:read-object expr)
	(signature-exploded sig)))))

(add-list-micro cu/s-unit-path-linkage-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (cu/s-check-self-import tag attributes)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids))
		       (small-sig
			 (expand-expr sig env attributes sig-vocab)))
		  (verify-signature-match 'compound-unit/sig
		    #f
		    (format "signature ~s" (signature-name small-sig))
		    (signature-exploded small-sig)
		    (format "signature ~s" (signature-name final-sig))
		    (signature-exploded final-sig))
		  (cons (z:read-object tag)
		    (signature-exploded small-sig)))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? tag)
	      (cu/s-check-self-import tag attributes)
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
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (valid-syntactic-id? tag)
	      (cu/s-check-self-import tag attributes)
	      (map valid-syntactic-id? ids)
	      (let ((initial-sig
		      (tag-table-entry-signature
			(cu/s-tag-table-lookup/static-error
			  (extract-cu/s-tag-table attributes) tag))))
		(let ((final-sig
			(extract-sub-unit-signature initial-sig ids)))
		  (cons (z:read-object tag)
		    (signature-exploded final-sig)))))))
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-unit-path-prim-links-vocab
  (create-vocabulary 'cu/s-unit-path-prim-links-vocab))

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
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
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
			  (cu/s-build-link-prefix ids tag)
			  ":")))
		    ((tag-table-link-entry? tag-table-entry)
		      (list
			(cons (z:read-object tag)
			  (cu/s-build-link-names small-sig
			    (cu/s-build-link-prefix ids)))))
		    (else
		      (internal-error tag-table-entry
			"Illegal tag-table entry"))))))))
	((pat:match-against m&e-2 expr env)
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
	((pat:match-against m&e-3 expr env)
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
			  (string-append
			    (cu/s-build-link-prefix ids tag)
			    ":")))
		      ((tag-table-link-entry? tag-table-entry)
			(list
			  (cons (z:read-object tag)
			    (cu/s-build-link-names final-sig
			      (cu/s-build-link-prefix ids)))))
		      (else
			(internal-error tag-table-entry
			  "Illegal tag-table entry")))))))))
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-unit-path-tag+build-prefix-vocab
  (create-vocabulary 'cu/s-unit-path-tag+build-prefix-vocab))

; Returns a pair of values:
; - Prefix tag of unit-path as Scheme symbol
; - String representing unit-path with ":" interspersed

(add-sym-micro cu/s-unit-path-tag+build-prefix-vocab
  (lambda (expr env attributes vocab)
    (cons (z:read-object expr)
      "")))

(add-list-micro cu/s-unit-path-tag+build-prefix-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (cons (z:read-object tag)
		(apply symbol-append
		  (let loop ((ids ids))
		    (if (null? ids) '("")
		      (if (null? (cdr ids))
			(list (z:read-object (car ids)))
			(cons (z:read-object (car ids))
			  (cons ":"
			    (loop (cdr ids))))))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (cons (z:read-object tag)
		""))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (cons (z:read-object tag)
		(apply symbol-append
		  (let loop ((ids ids))
		    (if (null? ids) '("")
		      (if (null? (cdr ids))
			(list (z:read-object (car ids)))
			(cons (z:read-object (car ids))
			  (cons ":"
			    (loop (cdr ids))))))))))))
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-unit-path-tag-vocab
  (create-vocabulary 'cu/s-unit-path-tag-vocab))

; Returns prefix tag of unit-path as Scheme symbol

(add-sym-micro cu/s-unit-path-tag-vocab
  (lambda (expr env attributes vocab)
    (z:read-object expr)))

(add-list-micro cu/s-unit-path-tag-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '((tag id ...) : sig))
	  (in-pattern-2 '(tag : sig))
	  (in-pattern-3 '(tag id ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd))
	  (m&e-3 (pat:make-match&env in-pattern-3 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd))
		   (sig (pat:pexpand 'sig p-env kwd)))
	      (cons (z:read-object tag)
		(apply symbol-append
		  (let loop ((ids ids))
		    (if (null? ids) '("")
		      (if (null? (cdr ids))
			(list (z:read-object (car ids)))
			(cons (z:read-object (car ids))
			  (cons ":"
			    (loop (cdr ids))))))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd)))
	      (z:read-object tag))))
	((pat:match-against m&e-3 expr env)
	  =>
	  (lambda (p-env)
	    (let ((tag (pat:pexpand 'tag p-env kwd))
		   (ids (pat:pexpand '(id ...) p-env kwd)))
	      (z:read-object tag))))
	(else
	  (static-error expr "Malformed unit path element"))))))

(define cu/s-build-link-names
  (opt-lambda (signature (prefix-string ""))
    (convert-to-prim-format-helper (signature-elements signature)
      prefix-string)))

(define cu/s-build-link-prefix
  (opt-lambda (ids (tag #f))
    (if (null? ids)
      ""
      (apply string-append
	(let ((result (let loop ((str-ids (map symbol->string
					    (map z:read-object ids))))
			(if (null? (cdr str-ids))
			  (list (car str-ids))
			  (cons (car str-ids)
			    (cons ":"
			      (loop (cdr str-ids))))))))
	  (if tag
	    (cons (symbol->string (z:read-object tag))
	      (cons ":"
		result))
	    result))))))

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

(define cu/s-prim-export-vocab
  (create-vocabulary 'cu/s-prim-export-vocab))

; Returns a fully-formed export element of the form
;   (tag (internal-name external-name))
; where each is a symbol or a z:symbol

(define prefix-w/-:
  (lambda (prefix name)
    (cond
      ((symbol? prefix)
	(if (string=? "" (symbol->string prefix))
	  name
	  (symbol-append prefix ":" name)))
      ((string? prefix)
	(if (string=? "" prefix)
	  name
	  (symbol-append prefix ":" name)))
      (else
	(internal-error 'prefix-w/-: "Got ~s as prefix" prefix)))))

(add-micro-form 'var cu/s-prim-export-vocab
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
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab)))
		(cons (car tag+prefix)
		  (list (list (prefix-w/-: (cdr tag+prefix)
				(z:read-object variable))
			  variable)))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd))
		   (external (pat:pexpand 'external-variable p-env kwd)))
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab)))
		(cons (car tag+prefix)
		  (list (list (prefix-w/-: (cdr tag+prefix)
				(z:read-object variable))
			  external)))))))
	(else
	  (static-error expr "Malformed var export"))))))

(add-micro-form 'open cu/s-prim-export-vocab
  (let* ((kwd '(open))
	  (in-pattern '(open unit-path))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(cons (car tag+prefix)
		  (map list
		    (convert-to-prim-format
		      (signature-elements final-sig)
		      (cdr tag+prefix))
		    (convert-to-prim-format
		      (signature-elements final-sig))))))))
	(else
	  (static-error expr "Malformed open export"))))))

(add-micro-form 'unit cu/s-prim-export-vocab
  (let* ((kwd '(unit))
	  (in-pattern-1 '(unit unit-path))
	  (in-pattern-2 '(unit unit-path variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(cons (car tag+prefix)
		  (map list
		    (convert-to-prim-format (signature-elements final-sig)
		      (cdr tag+prefix))
		    (convert-to-prim-format (signature-elements final-sig)
		      (car tag+prefix))))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (valid-syntactic-id? variable)
	      (let ((tag+prefix
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag+build-prefix-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(cons (car tag+prefix)
		  (map list
		    (convert-to-prim-format (signature-elements final-sig)
		      (cdr tag+prefix))
		    (convert-to-prim-format (signature-elements final-sig)
		      (z:read-object variable))))))))
	(else
	  (static-error expr "Malformed unit export"))))))

(define cu/s-export-sign-vocab
  (create-vocabulary 'cu/s-export-sign-vocab))

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
	      (list variable))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd))
		   (external (pat:pexpand 'external-variable p-env kwd)))
	      (list external))))
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
		(signature-exploded final-sig)))))
	(else
	  (static-error expr "Malformed open export"))))))

(add-micro-form 'unit cu/s-export-sign-vocab
  (let* ((kwd '(unit))
	  (in-pattern-1 '(unit unit-path))
	  (in-pattern-2 '(unit unit-path variable))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd)))
	      (let ((tag
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(list (cons tag
			(signature-exploded final-sig)))))))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((unit-path (pat:pexpand 'unit-path p-env kwd))
		   (variable (pat:pexpand 'variable p-env kwd)))
	      (let ((tag
		      (expand-expr unit-path env attributes
			cu/s-unit-path-tag-vocab))
		     (final-sig
		       (expand-expr unit-path env attributes
			 cu/s-unit-path-extract-final-sig-vocab)))
		(list (cons (z:read-object variable)
			(signature-exploded final-sig)))))))
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

(add-primitivized-micro-form 'compound-unit/sig scheme-vocabulary
  (let* ((kwd '(import link export))
	  (in-pattern '(_
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
		      (prim:exports (map (lambda (e)
					   (expand-expr e env attributes
					     cu/s-prim-export-vocab))
				      in:exports))
		      (sign:imports (map (lambda (i)
					   (expand-expr i env attributes
					     cu/s-sign-imports-vocab))
				      in:imports))
		      (sign:exports (apply append
				      (map (lambda (e)
					     (expand-expr e env attributes
					       cu/s-export-sign-vocab))
					in:exports))))
		(let ((output
			`(let ,(map list linkage:unit-vars linkage:unit-exprs)
			   (#%verify-linkage-signature-match
			     'compound-unit/sig
			     ',linkage:tags
			     (#%list ,@linkage:unit-vars)
			     ',linkage:link-exports
			     ',linkage:link-imports)
			   (#%make-unit-with-signature
			     (compound-unit
			       (import ,@prim:imports)
			       (link ,@(map (lambda (tag body)
					      `(,tag
						 ((#%unit-with-signature-unit
						    ,tag)
						   ,@body)))
					 linkage:tags prim:links))
			       (export ,@prim:exports))
			     ',sign:imports
			     ',sign:exports))))
		  (expand-expr
		    (structurize-syntax
		      output
		      expr)
		    env attributes vocab))))))
	(else
	  (static-error expr "Malformed compound-unit/sig"))))))

; --------------------------------------------------------------------

(define iu/s-linkage-vocab
  (create-vocabulary 'iu/s-linkage-vocab))

(add-sym-micro iu/s-linkage-vocab
  (lambda (expr env attributes vocab)
    (cons expr
      (signature-exploded (expand-expr expr env attributes sig-vocab)))))

(add-list-micro iu/s-linkage-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((in:id (pat:pexpand 'id p-env kwd))
		   (in:sig (pat:pexpand 'sig p-env kwd)))
	      (valid-syntactic-id? in:id)
	      (cons (z:read-object in:id)
		(signature-exploded
		  (expand-expr in:sig env attributes sig-vocab))))))
	((pat:match-against m&e-2 expr env)
	  (static-error expr "Ambiguous : in signature"))
	(else
	  (cons immediate-signature-name
	    (signature-exploded
	      (expand-expr expr env attributes sig-vocab))))))))

(define iu/s-imports-vocab
  (create-vocabulary 'iu/s-imports-vocab))

(add-sym-micro iu/s-imports-vocab
  (lambda (expr env attributes vocab)
    (convert-to-prim-format
      (signature-elements (expand-expr expr env attributes sig-vocab)))))

(add-list-micro iu/s-imports-vocab
  (let* ((kwd '(:))
	  (in-pattern-1 '(id : sig))
	  (in-pattern-2 '(id : any ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (let ((in:id (pat:pexpand 'id p-env kwd))
		   (in:sig (pat:pexpand 'sig p-env kwd)))
	      (convert-to-prim-format
		(signature-elements
		  (expand-expr in:sig env attributes sig-vocab))
		(z:read-object in:id)))))
	((pat:match-against m&e-2 expr env)
	  (static-error expr "Ambiguous : in signature"))
	(else
	  (convert-to-prim-format
	    (signature-elements
	      (expand-expr expr env attributes sig-vocab))))))))

(add-primitivized-micro-form 'invoke-unit/sig scheme-vocabulary
  (let* ((kwd '())
	  (in-pattern '(_ expr linkage ...))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((in:expr (pat:pexpand 'expr p-env kwd))
		   (in:linkage (pat:pexpand '(linkage ...) p-env kwd)))
	      (let ((proc:linkage (map (lambda (l)
					 (expand-expr l env attributes
					   iu/s-linkage-vocab))
				    in:linkage))
		     (proc:imports (apply append
				     (map (lambda (l)
					    (expand-expr l env attributes
					      iu/s-imports-vocab))
				       in:linkage))))
		(expand-expr
		  (structurize-syntax
		    `(let ((unit ,in:expr))
		       (#%verify-linkage-signature-match
			 'invoke-unit/sig
			 '(invoke)
			 (#%list unit)
			 '(())
			 '(,proc:linkage))
		       (invoke-unit
			 (#%unit-with-signature-unit unit)
			 ,@proc:imports))
		    expr)
		  env attributes vocab)))))
	(else
	  (static-error expr "Malformed invoke-unit/sig"))))))

(add-primitivized-micro-form 'invoke-open-unit/sig scheme-vocabulary
  (let* ((kwd '())
	  (in-pattern-1 '(_ expr))
	  (out-pattern-1 '(invoke-open-unit/sig expr #f))
	  (in-pattern-2 '(_ expr name-spec linkage ...))
	  (m&e-1 (pat:make-match&env in-pattern-1 kwd))
	  (m&e-2 (pat:make-match&env in-pattern-2 kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e-1 expr env)
	  =>
	  (lambda (p-env)
	    (expand-expr
	      (structurize-syntax
		(pat:pexpand out-pattern-1 p-env kwd)
		expr)
	      env attributes vocab)))
	((pat:match-against m&e-2 expr env)
	  =>
	  (lambda (p-env)
	    (let ((in:expr (pat:pexpand 'expr p-env kwd))
		   (in:name-spec (pat:pexpand 'name-spec p-env kwd))
		   (in:linkage (pat:pexpand '(linkage ...) p-env kwd)))
	      (let ((proc:linkage (map (lambda (l)
					 (expand-expr l env attributes
					   iu/s-linkage-vocab))
				    in:linkage))
		     (proc:imports (apply append
				     (map (lambda (l)
					    (expand-expr l env attributes
					      iu/s-imports-vocab))
				       in:linkage))))
		(expand-expr
		  (structurize-syntax
		    `(let ((unit ,in:expr))
		       (#%verify-linkage-signature-match
			 'invoke-open-unit/sig
			 '(invoke)
			 (#%list unit)
			 '(())
			 '(,proc:linkage))
		       (invoke-open-unit
			 (#%unit-with-signature-unit unit)
			 ,in:name-spec
			 ,@proc:imports))
		    expr)
		  env attributes vocab)))))
	(else
	  (static-error expr "Malformed invoke-open-unit/sig"))))))

(add-primitivized-micro-form 'unit->unit/sig scheme-vocabulary
  (let* ((kwd '())
	  (in-pattern '(_ expr (in-sig ...) out-sig))
	  (m&e (pat:make-match&env in-pattern kwd)))
    (lambda (expr env attributes vocab)
      (cond
	((pat:match-against m&e expr env)
	  =>
	  (lambda (p-env)
	    (let ((in-expr (pat:pexpand 'expr p-env kwd))
		   (in-sigs (pat:pexpand '(in-sig ...) p-env kwd))
		   (out-sig (pat:pexpand 'out-sig p-env kwd)))
	      (expand-expr
		(structurize-syntax
		  `(#%make-unit-with-signature
		     ,in-expr
		     ',(map (lambda (s)
			      (let ((proc:s
				      (expand-expr s env attributes
					sig-vocab)))
				(cons (signature-name proc:s)
				  (signature-exploded proc:s))))
			 in-sigs)
		     ',(let ((proc:s
			       (expand-expr out-sig env attributes sig-vocab)))
			 (signature-exploded proc:s)))
		  expr)
		env attributes vocab))))
	(else
	  (static-error expr "Malformed unit->unit/sig"))))))
