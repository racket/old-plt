(unit/sig before^
  (import countdown^
	  [mred : mred^])

  (mred:set-preference-default 'countdwn:frame-size
			       (list 500 300)
			       (lambda (x)
				 (and (list? x)
				      (= 2 (length x))
				      (andmap number? x))))

  (define frame (make-object (class-asi mred:frame%
			       (rename [super-on-close on-close]
				       [super-on-size on-size])
			       (public
				 [on-size
				  (lambda (w h)
				    (mred:set-preference 'countdwn:frame-size
							 (list w h))
				    (super-on-size w h))]
				 [on-close
				  (lambda () (and (super-on-close)
						  (mred:exit)
						  (send edit shutdown)))]))
		  null "Countdown"))
  (define panel (make-object mred:vertical-panel% frame))
  (define canvas (make-object mred:wide-snip-canvas% panel))
  (define edit (make-object main-edit%))

  (send canvas set-media edit)
  (let ([size-list (mred:get-preference 'countdwn:frame-size)]) 
    (send frame set-size 0 0 (car size-list) (cadr size-list))))
