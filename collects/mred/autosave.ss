(unit/sig mred:autosave^
  (import [wx : wx^]
	  [mred:constants : mred:constants^]
	  [mred:exit : mred:exit^]
	  [mred:preferences : mred:preferences^])
  
  (mred:debug:printf 'invoke "mred:autosave@")
  
  (mred:preferences:set-preference-default 'mred:autosave-delay 300 number?)
  (mred:preferences:set-preference-default 'mred:autosaving-on? #t 
					   (lambda (x)
					     (or (not x)
						 (eq? x #t))))
  
  (define register-autosave
    (let* ([objects null]
	   [timer
	    (make-object
	     (class wx:timer% ()
	       (inherit start)
	       (public
		 [notify
		  (lambda ()
		    (mred:debug:printf 'autosave "autosaving notified")
		    (when (mred:preferences:get-preference 'mred:autosaving-on?)
		      (set! objects
			    (let loop ([list objects])
			      (if (null? list)
				  null
				  (let ([object (weak-box-value (car list))])
				    (if object
					(begin
					  (mred:debug:printf 'autosave
							     "autosaving: ~a"
							     object)
					  (send object do-autosave)
					  (cons (car list) (loop (cdr list))))
					(loop (cdr list))))))))
		    (let ([seconds (mred:preferences:get-preference 'mred:autosave-delay)])
		      (mred:debug:printf 'autosave 
					 "autosaving restarting in ~a seconds"
					 seconds)
		      (start (* 1000 seconds) #t)))])
	       (sequence
		 (super-init)
		 (let ([seconds (mred:preferences:get-preference
				 'mred:autosave-delay)])
		   (mred:debug:printf 'autosave "autosaving starting, notifing in ~a seconds"
				      seconds)
		   (start (* 1000 seconds) #t)))))])
      (lambda (b)
	(mred:debug:printf 'autosave "registering ~a" b)
	(set! objects
	      (let loop ([objects objects])
		(cond
		  [(null? objects) (list (make-weak-box b))]
		  [else (let ([weak-box (car objects)])
			  (if (weak-box-value weak-box)
			      (cons weak-box (loop (cdr objects)))
			      (loop (cdr objects))))])))))))


