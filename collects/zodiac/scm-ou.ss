; $Id$

(unit/sig zodiac:scheme-objects+units^
  (import zodiac:misc^ (z : zodiac:structures^) (z : zodiac:reader-structs^)
    zodiac:sexp^ (pat : zodiac:pattern^) 
    zodiac:expander^ zodiac:interface^
    zodiac:scheme-core^ zodiac:scheme-main^
    zodiac:scheme-objects^ zodiac:scheme-units^)

  (let ((handler
	  (lambda (expr env attributes vocab)
	    (let ((r (resolve expr env vocab)))
	      (cond
		((lexical-binding? r)
		  (create-lexical-varref r expr))
		((top-level-resolution? r)
		  (let ((id (z:read-object expr)))
		    (unless (built-in-name id)
		      (update-unresolved-attribute attributes expr))
		    (let ((top-level-space (get-attribute attributes
					     'top-levels)))
		      (if top-level-space
			(begin
			  (let ((ref
				  (create-top-level-varref/bind
				    id
				    (hash-table-get top-level-space id
				      (lambda ()
					(let ((b (box '())))
					  (hash-table-put! top-level-space id b)
					  b)))
				    expr)))
			    (let ((b (top-level-varref/bind-slot ref)))
			      (set-box! b (cons ref (unbox b))))
			    ref))
			(create-top-level-varref id expr)))))
		((public-binding? r)
		  (create-public-varref r expr))
		((private-binding? r)
		  (create-private-varref r expr))
		((inherit-binding? r)
		  (create-inherit-varref r expr))
		((rename-binding? r)
		  (create-rename-varref r expr))
		((supervar-binding? r)
		  (create-supervar-varref r expr))
		((superinit-binding? r)
		  (create-superinit-varref r expr))
		((or (macro-resolution? r) (micro-resolution? r))
		  (static-error expr
		    "Invalid use of keyword ~s" (z:symbol-orig-name expr)))
		(else
		  (internal-error expr "Invalid resolution in ou: ~s" r)))))))
    (add-sym-micro scheme-vocabulary handler)
    (add-sym-micro unit-clauses-vocab-delta handler))

  )
