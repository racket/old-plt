(unit/sig stepper:unparse^
  (import [z : zodiac:system^]
	  stepper:error)
  
  ; copied from aries
  
  (define read->raw
    (lambda (read)
      (if (z:zodiac? read)
	  (z:sexp->raw read)
	  read)))
 
  (define arglist->ilist
    (lambda (arglist)
      (cond
	((z:list-arglist? arglist)
	 (z:arglist-vars arglist))
	((z:ilist-arglist? arglist)
	 (let loop ((vars (z:arglist-vars arglist)))
	   (if (null? (cddr vars))
	       (cons (car vars) (cadr vars))
	       (cons (car vars) (loop (cdr vars))))))
	((z:sym-arglist? arglist)
	 (car (z:arglist-vars arglist)))
	(else
	 (e:internal-error arglist
			   "Given to arglist->ilist")))))
  
)