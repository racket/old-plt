(unit/sig mred:application^
  (import [mred : mred^]
	  [core : mzlib:core^])
  (define app-name "MrEd")
  (define console #f) ; for nu, console is created later
  (define eval-string (lambda (s)
			(if console
			    (let ([ce (send console get-edit)])
			      (send ce eval-and-display s)
			      (send ce insert-prompt)
			      #t)
			    #f)))
  (define startup (lambda args 
		    (set! console (make-object mred:console-frame%))
		    (for-each mred:edit-file args)
		    console)))


