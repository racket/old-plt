
(define-sigfunctor (mred:autosave@ mred:autosave^)
  (import mred:debug^)
  
  (define autosave-delay 60)
  (define autosaving-on? #t)

  (define register-autosave
    (let* ([objects '()]
	   [timer
	    (make-object
	     (class wx:timer% ()
	       (inherit start)
	       (public
		[notify
		 (lambda ()
		   (if autosaving-on?
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
		   (if (not (number? autosave-delay))
		       (set! autosave-delay 300))
		   (start (* 1000 autosave-delay) #t))])
	       (sequence
		 (super-init)
		 (if (not (number? autosave-delay))
		     (set! autosave-delay 300))
		 (start (* 1000 autosave-delay) #t))))])
      (lambda (b)
	(set! objects
	      (cons (make-weak-box b) objects)))))
  )


