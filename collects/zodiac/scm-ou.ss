; $Id: scm-ou.ss,v 1.11 1998/03/04 22:07:52 shriram Exp $

(unit/sig zodiac:scheme-objects+units^
  (import zodiac:misc^ (z : zodiac:structures^) (z : zodiac:reader-structs^)
    zodiac:sexp^ (pat : zodiac:pattern^) 
    zodiac:expander^ zodiac:interface^
    zodiac:scheme-core^ zodiac:scheme-main^
    zodiac:scheme-objects^ zodiac:scheme-units^)

  (let ((handler
	  (let ((top-level-resolution (make-top-level-resolution 'dummy #f)))
	    (lambda (expr env attributes vocab)
	      (let loop ((r (resolve expr env vocab)))
		(cond
		  ((lexical-binding? r)
		    (create-lexical-varref r expr))
		  ((top-level-resolution? r)
		    (let ((id (z:read-object expr)))
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
			      (unless (built-in-name id)
				(update-unresolved-attribute attributes expr ref))
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
		    (if (and (inside-unit? attributes)
			  (check-export expr attributes))
		      (loop top-level-resolution)
		      (static-error expr
			"Invalid use of keyword ~a" (z:symbol-orig-name expr))))
		  (else
		    (internal-error expr "Invalid resolution in ou: ~s" r))))))))
    (add-sym-micro scheme-vocabulary handler)
    (add-sym-micro unit-clauses-vocab-delta handler))

  )
