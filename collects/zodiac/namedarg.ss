(reference-library "match.ss")
(reference-library "macro.ss")

(begin-elaboration-time

(invoke-open-unit

(unit (import)
  (export define-argument-list lambda/nal call/nal)
  
  (define-struct argument-list-entry ())
  (define-struct (flat-argument-list-entry struct:argument-list-entry) ())
  (define-struct (var-argument struct:flat-argument-list-entry) (var))
  (define-struct (kwd-argument struct:flat-argument-list-entry) (kwd var))
  (define-struct (opt-argument struct:argument-list-entry) (arg val))
  (define argument-list-table
    (make-hash-table))
  (define-struct argument-list-table-entry
    (marker vars kwds var-opts kwd-opts))

  (define extract-argument-list-var-name
    (lambda (arg)
      (cond
	((var-argument? arg) (var-argument-var arg))
	((kwd-argument? arg) (kwd-argument-var arg))
	((opt-argument? arg) (extract-argument-list-var-name
			       (opt-argument-arg arg)))
	(else
	  (error 'extract-argument-list-var-name
	    "Invalid argument: ~s" arg)))))

  (define arglist-check-context-sensitive-properties
    (lambda (parseds)
      (let ((vars (map extract-argument-list-var-name parseds)))
	(let loop ((vars vars))
	  (unless (null? vars)
	    (if (memq (car vars) (cdr vars))
	      (error 'define-argument-list "Duplicate argument: ~s"
		(car vars))
	      (loop (cdr vars))))))))

  (define get-argument-list
    (lambda (al-name caller)
      (let ((al-entry (hash-table-get argument-list-table al-name
			(lambda () (error caller
				     "No such argument list: ~s" al-name)))))
	(values
	  (argument-list-table-entry-marker al-entry)
	  (argument-list-table-entry-vars al-entry)
	  (argument-list-table-entry-kwds al-entry)
	  (argument-list-table-entry-var-opts al-entry)
	  (argument-list-table-entry-kwd-opts al-entry)))))

  (define define-argument-list
    (letrec
      ((parse-argument
	 (match-lambda
	   ((and (? symbol?) v)
	     (make-var-argument v))
	   (('kwd (and (? symbol?) kwd) (and (? symbol?) var))
	     (make-kwd-argument kwd var))
	   (('opt arg val)
	     (let ((a (parse-argument arg)))
	       (if (flat-argument-list-entry? a)
		 (make-opt-argument (parse-argument arg)
		   val)
		 (error 'define-argument-list
		   "Invalid nesting of arguments: ~s" arg))))
	   (wrong
	     (error 'define-argument-list "Invalid entry: ~s" wrong)))))
      (lambda (name . arguments)
	(let ((parseds (map parse-argument arguments)))
	  (arglist-check-context-sensitive-properties parseds)
	  (let loop ((parseds parseds)
		      (vars '())
		      (kwds '())
		      (var-opts '())
		      (kwd-opts '()))
	    (if (null? parseds)
	      (hash-table-put! argument-list-table name
		(make-argument-list-table-entry
		  (string->uninterned-symbol
		    (symbol->string name))
		  (reverse vars)
		  kwds var-opts kwd-opts))
	      (let ((first (car parseds))
		     (rest (cdr parseds)))
		(cond
		  ((var-argument? first)
		    (loop rest (cons first vars) kwds var-opts kwd-opts))
		  ((kwd-argument? first)
		    (loop rest vars (cons first kwds) var-opts kwd-opts))
		  ((opt-argument? first)
		    (cond
		      ((var-argument? (opt-argument-arg first))
			(loop rest vars kwds (cons first var-opts) kwd-opts))
		      ((kwd-argument? (opt-argument-arg first))
			(loop rest vars kwds var-opts (cons first kwd-opts)))
		      (else
			(error 'define-argument-list "Invalid argument: ~s"
			  first))))
		  (else
		    (error 'define-argument-list "Invalid argument: ~s"
		      first))))))))))

  (define lambda/nal
    (lambda (al-name . body)
      (let-values (((marker vars kwds opt-vars opt-kwds)
		     (get-argument-list al-name 'lambda/nal)))
	(let ((requireds (append (map var-argument-var vars)
			   (map kwd-argument-var kwds))))
	  `(opt-lambda (,marker
			 ,@(append
			     requireds
			     (map (lambda (opt-var)
				    (list (var-argument-var
					    (opt-argument-arg opt-var))
				      (opt-argument-val opt-var)))
			       opt-vars)
			     (map (lambda (opt-kwd)
				    (list (kwd-argument-var
					    (opt-argument-arg opt-kwd))
				      (opt-argument-val opt-kwd)))
			       opt-kwds)))
	     (unless (eq? ,marker ',marker)
	       (error 'lambda/al "Type ~s arguments given type ~s"
		 ',marker ,marker))
	     ,@body)))))

  (define call/nal
    (lambda (al-name function . args)
      (let-values (((marker vars kwds opt-vars opt-kwds)
		     (get-argument-list al-name 'call/nal)))
	(let ((kwd-tags (append (map kwd-argument-kwd kwds)
			  (map (lambda (a)
				 (kwd-argument-kwd
				   (opt-argument-arg a)))
			    opt-kwds))))
	  (let loop ((args args)
		      (non-kwd '())
		      (kwd '()))
	    (if (null? args)
	      (let ((var-actuals (reverse non-kwd))
		     (kwd-actuals kwd))	; in typical case, reverse might help
		(cond
		  ((< (length var-actuals) (length vars))
		    (error 'call/nal "Insufficient arguments for ~s" al-name))
		  ((< (length kwd-actuals) (length kwds))
		    (error 'call/nal "Insufficient keyword arguments for ~s"
		      al-name))
		  (else
		    `(,function
		       ',marker
		       ,@(append
			   (let loop ((vars vars)
				       (var-actuals var-actuals))
			     (if (null? vars)
			       (let loop ((kwds kwds))
				 (if (null? kwds)
				   (let loop ((opt-vars opt-vars)
					       (var-actuals var-actuals))
				     (if (null? opt-vars)
				       (let loop ((opt-kwds opt-kwds))
					 (if (null? opt-kwds)
					   (if (null? var-actuals)
					     null
					     (error 'call/nal
					       "Too many actuals: ~s unused"
					       var-actuals))
					   (let ((entry (assq
							  (kwd-argument-kwd
							    (opt-argument-arg
							      (car opt-kwds)))
							  kwd-actuals)))
					     (cons (if entry
						     (cdr entry)
						     (opt-argument-val
						       (car opt-kwds)))
					       (loop (cdr opt-kwds))))))
				       (if (null? var-actuals)
					 (cons (opt-argument-val (car opt-vars))
					   (loop (cdr opt-vars) null))
					 (cons (car var-actuals)
					   (loop (cdr opt-vars)
					     (cdr var-actuals))))))
				   (cons (let ((entry (assq (kwd-argument-kwd
							      (car kwds))
							kwd-actuals)))
					   (if entry
					     (cdr entry)
					     (error 'call/nal
					       "No argument for keyword ~s"
					       (kwd-argument-kwd (car kwds)))))
				     (loop (cdr kwds)))))
			       (cons (car var-actuals)
				 (loop (cdr vars) (cdr var-actuals))))))))))
	      (match args
		(((and (? symbol?) first) . rest)
		  (loop rest (cons first non-kwd) kwd))
		((((and (? (lambda (k)
			     (and (symbol? k)
			       (memq k kwd-tags)))) k) expr) . rest)
		  (loop rest non-kwd (cons (cons k expr) kwd)))
		((((and (? (lambda (k)
			     (and (symbol? k)
			       (memq k kwd-tags)))) k) . expr) . rest)
		  (error 'call/nal "~s invalid binding for keyword ~s" expr k))
		((first . rest)
		  (loop rest (cons first non-kwd) kwd))
		(other
		  (error 'call/nal "Malformed argument ~s" other))))))))))

)					; invoke-open-unit

)					; begin-elaboration-time

(define-macro define-argument-list
  (begin-elaboration-time define-argument-list))

(define-macro lambda/nal
  (begin-elaboration-time lambda/nal))

(define-macro call/nal
  (begin-elaboration-time call/nal))

#|

(define-argument-list m3-elaborator
  expr foo (kwd attributes: attr) (kwd vocabulary: vocab)
  (opt (kwd parameterization: params) (current-parameterization))
  (opt other 3))

(define-argument-list other
  expr foo (kwd attributes: attr) (kwd vocabulary: vocab)
  (opt (kwd parameterization: params) (current-parameterization))
  (opt other 3))

(define f (lambda/nal m3-elaborator
	    (values expr foo attr vocab params other)))

(call/nal m3-elaborator f
  (parameterization: 'pahram)
  (vocabulary: 'vohcab )
  'exper
  'fuh
  'udder
  (attributes: 'atter))

|#
