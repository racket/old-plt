
  (unit/sig mred:autosave^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:exit : mred:exit^]
	    [mred:preferences : mred:preferences^])
	    
    (mred:debug:printf 'invoke "mred:autosave@")

    (mred:preferences:set-preference-default 'mred:autosave-delay 300)
    (mred:preferences:set-preference-default 'mred:autosaving-on? #t)

    (define register-autosave
      (let* ([objects '()]
	     [timer
	      (make-object
		  (class wx:timer% ()
			 (inherit start)
			 (public
			  [notify
			   (lambda ()
			     (if (mred:preferences:get-preference 'mred:autosaving-on?)
				 (set! objects
				       (let loop ([list objects])
					 (if (null? list)
					     ()
					     (let ([object (weak-box-value (car list))])
					       (if object
						   (begin
						     (send object do-autosave)
						     (cons (car list) (loop (cdr list))))
						   (loop (cdr list))))))))
			     (start (* 1000 (mred:preferences:get-preference 'mred:autosave-delay)) #t))])
			 (sequence
			   (super-init)
			   (start (* 1000
				     (mred:preferences:get-preference
				      'mred:autosave-delay))
				  #t))))])
	(mred:exit:insert-exit-callback
	 (lambda ()
	   (send timer stop)))
	(lambda (b)
	  (set! objects
		(cons (make-weak-box b) objects))))))


