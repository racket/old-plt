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
  (define canvas (make-object mred:wide-snip-canvas% panel))
  (define edit (make-object main-edit%))

  (send canvas set-media edit)
  (send frame set-size 0 0 500 300)
  (send edit begin-edit-sequence))
