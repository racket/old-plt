(cons
 (unit/sig (force)
   (import plt:userspace^)

   (define (force thunk)
     (thunk)))
 (unit/sig (delay)
   (import plt:userspace^)
   (define (delay delayed-expression)
     (let ([thunk (gensym "delay-thunk")])
       `(letrec ([,thunk
		  (lambda ()
		    (let ([orig-thunk ,thunk]
			  [answer ,delayed-expression])
		      (when (eq? orig-thunk ,thunk)
			(set! ,thunk (lambda () answer))
			answer)))])
	  (lambda () (,thunk)))))))
