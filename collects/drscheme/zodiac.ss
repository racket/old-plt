(begin-elaboration-time
 (let* ([get-signature
	 (lambda (name)
	   (let ([e (eval `(let-id-macro 
			    x 
			    `',(global-expansion-time-value ',name) 
			    x))])
	     e))]
	[all-names (get-signature 'zodiac:system^)]
	[non-function-names
	 '(scheme-vocabulary
	   arglist-decls-vocab
	   paroptarglist-decls-vocab
	   mrspidey-vocabulary
	   optarglist-decls-vocab
	   unit-clauses-vocab-delta

	   optarglist-pattern
	   paroptarglist-pattern
	   arglist-pattern

	   default-initial-location
	   mzscheme-libraries-provided
	   m3-elaboration-evaluator
	   m3-macro-body-evaluator

	   scan:def-first-col
	   scan:def-vect-val
	   scan:delim-list
	   scan:newline-list
	   scan:paren-relation
	   scan:self-delim-symbols
	   scan:special-char-list
	   scan:tab-list
	   scan:whitespace-list

	   struct::-form
	   struct:app
	   struct:arglist
	   struct:begin-form
	   struct:begin0-form
	   struct:binding
	   struct:boolean
	   struct:bound-varref
	   struct:box
	   struct:case-lambda-form
	   struct:char
	   struct:class*/names-form
	   struct:compound-unit-form
	   struct:define-constructor-form
	   struct:define-type-form
	   struct:define-values-form
	   struct:eof
	   struct:external
	   struct:form
	   struct:if-form
	   struct:ilist-arglist
	   struct:ilist-optarglist
	   struct:ilist-paroptarglist
	   struct:improper-list
	   struct:inherit-binding
	   struct:inherit-clause
	   struct:inherit-varref
	   struct:initialized-optarglist-entry
	   struct:initialized-paroptarglist-entry
	   struct:interface-form
	   struct:invoke-open-unit-form
	   struct:invoke-unit-form
	   struct:let-values-form
	   struct:letrec*-values-form
	   struct:lexical-binding
	   struct:lexical-varref
	   struct:list
	   struct:list-arglist
	   struct:list-optarglist
	   struct:list-paroptarglist
	   struct:location
	   struct:number
	   struct:optarglist
	   struct:optarglist-entry
	   struct:origin
	   struct:paroptarglist
	   struct:paroptarglist-entry
	   struct:parsed
	   struct:period
	   struct:poly-form
	   struct:private-binding
	   struct:private-clause
	   struct:private-varref
	   struct:public-binding
	   struct:public-clause
	   struct:public-varref
	   struct:quote-form
	   struct:read
	   struct:reference-unit-form
	   struct:rename-binding
	   struct:rename-clause
	   struct:rename-varref
	   struct:scalar
	   struct:sequence
	   struct:sequence-clause
	   struct:set!-form
	   struct:st:control-form
	   struct:string
	   struct:struct-form
	   struct:superinit-binding
	   struct:superinit-varref
	   struct:supervar-binding
	   struct:supervar-varref
	   struct:sym-arglist
	   struct:sym-optarglist
	   struct:sym-paroptarglist
	   struct:symbol
	   struct:top-level-varref
	   struct:top-level-varref/bind
	   struct:type-symbol
	   struct:type:-form
	   struct:unit-form
	   struct:varref
	   struct:vector
	   struct:vocabulary-record
	   struct:zodiac)]
	[bad-names-table
	 (let ([ht (make-hash-table)])
	   (for-each (lambda (x) (hash-table-put! ht x #t))
		     non-function-names)
	   ht)]
	[function-names
	 (foldl (lambda (x l)
		  (if (hash-table-get bad-names-table x (lambda () #f))
		      l
		      (cons x l)))
		null
		all-names)])
   `(unit/sig drscheme:zodiac^
      (import [mred : mred^]
	      [beginner : zodiac:system^]
	      [intermediate : zodiac:system^]
	      [advanced : zodiac:system^]
	      [quasi-r4rs : zodiac:system^]
	      [basis : drscheme:basis^])
      
      (define bad-names null)

      ,@(let loop ([other-names non-function-names])
	  (cond
	    [(null? other-names) null]
	    [else (let ([name (car other-names)])
		    (cons `(define ,name
			     ,(string-append "zodiac.ss;not-"
					     (symbol->string name)))
			  (loop (cdr other-names))))]))

      (define current-vocabulary
	(make-parameter 
	 'advanced
	 (lambda (x)
	   (if (member x basis:level-symbols)
	       (begin 
		 ,@(let loop ([other-names non-function-names])
		     (cond
		       [(null? other-names) null]
		       [else (let* ([name (car other-names)]
				    [prefix
				     (lambda (prefix)
				       (string->symbol (string-append prefix ":"
								      (symbol->string name))))])
			       (cons
				`(begin
				   (mred:debug:printf 'zodiac.ss "updating from ~a; ~a~n" 
						      x 
						      ',name)
				   (set! ,name
					 (case x
					   [(core) ,(prefix "beginner")]
					   [(structured) ,(prefix "intermediate")]
					   [(side-effecting) ,(prefix "advanced")]
					   [(advanced) ,(prefix "quasi-r4rs")])))
				(loop (cdr other-names))))]))
		 x)
	       (error 'current-vocabulary
		      "supplied non-vocab symbol: ~a" x)))))
      
      (define make-function
	(lambda (name core structured side-effecting advanced args)
	  (let ([vocab (current-vocabulary)])
	    (mred:debug:printf 'zodiac.ss "calling from ~a; ~a~n" vocab name)
	    (apply (case vocab
		     [(core) core]
		     [(structured) structured]
		     [(side-effecting) side-effecting]
		     [(advanced) advanced])
		   args))))
      
      ,@(let loop ([names function-names])
	  (cond
	    [(null? names) null]
	    [else 
	     (let* ([name (car names)]
		    [prefix
		     (lambda (x)
		       (string->symbol 
			(string-append 
			 x
			 ":"
			 (symbol->string name))))]
		    [test
		     `(unless (procedure? ,(prefix "beginner"))
			(set! bad-names (cons (cons ',name ,name) bad-names)))]
		    [defn
		     `(define (,name . args)
			(make-function ',name
				       ,(prefix "beginner")
				       ,(prefix "intermediate")
				       ,(prefix "advanced")
				       ,(prefix "quasi-r4rs")
				       args))])
	       (list* test defn (loop (cdr names))))]))
      
      (unless (null? bad-names)
	(printf "bad-names~n~a~n" bad-names)
	(error 'zodiac.ss "found non-procedural names"))

      (current-vocabulary 'advanced))))