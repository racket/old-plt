(unit/sig mred:application^
  (import [mred : mred^]
	  [core : mzlib:core^])
  (define app-name "MrEd")
  (define console (make-object mred:console-frame%))
  (define eval-string (lambda (s)
			(let ([ce (send console get-edit)])
			  (send ce eval-and-display s)
			  (send ce insert-prompt)
			  #t))))