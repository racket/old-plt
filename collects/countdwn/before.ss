(unit/sig before^
  (import countdown^
	  mred^)

  (define frame-size (list 500 300))

  '(mred:set-preference-default 'countdwn:frame-size
			       (list 500 300)
			       (lambda (x)
				 (and (list? x)
				      (= 2 (length x))
				      (andmap number? x))))

  (define quit-semaphore (make-semaphore 0))

  (define frame (make-object (class-asi frame%
			       (rename [super-on-close on-close]
				       [super-on-size on-size])
			       (override
				 [on-size
				  (lambda (w h)
				    '(mred:set-preference 'countdwn:frame-size
							 (list w h))
				    (super-on-size w h))]
				 [on-close
				  (lambda () (and (super-on-close)
						  (send edit shutdown)
						  (semaphore-post quit-semaphore)))]))
		  "Countdown"))
  (define panel (make-object vertical-panel% frame))
  (define canvas (make-object editor-canvas% panel))
  (define edit (make-object main-edit%))

  (send canvas set-edit edit)
  (let ([size-list (begin '(mred:get-preference 'countdwn:frame-size) frame-size)]) 
    (send frame min-width (car size-list))
    (send frame min-height (cadr size-list))))
