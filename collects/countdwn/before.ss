(unit/sig before^
  (import countdown^
	  [mred : mred^])

  (define frame (make-object (class-asi mred:frame%
			       (rename [super-on-close on-close])
			       (public
				 [on-close
				  (lambda () (and (super-on-close)
						  (mred:exit)
						  (send edit shutdown)))]))
		  null "Countdown"))
  (define panel (make-object mred:vertical-panel% frame))
  (define canvas (make-object mred:media-canvas% panel))
  (define edit (make-object main-edit%))

  (send* canvas 
    (set-media edit)
    (user-min-width 500)
    (user-min-height 200))
  (send edit begin-edit-sequence))
