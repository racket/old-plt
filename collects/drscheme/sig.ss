(unit/sig drscheme:signature^
  (import [mred : mred^])
  (define frame%
    (class mred:empty-frame% (filename)
      (inherit panel)
      (private
	[signatures-names null]
	[insert-signature-panel
	 (lambda (name)
	   (let ([vp (make-object mred:horizontal-panel% panel)]
		 [hp (make-object mred:horizontal-panel% vp)]
		 [body (make-object mred:media-canvas% vp)]
		 [edit (make-object mred:media-edit%)]
		 [button-callback
		  (let ([shown? #f])
		    (lambda (button)
		      (if shown?
			  (begin (send button set-label "Show")
				 (send vp change-children
				       (lambda (l)
					 (list hp))))
			  (begin (send button set-label "Hide")
				 (send vp change-children
				       (lambda (l)
					 (list hp body)))))
		      (set! shown? (not shown?))))])
	     (make-object mred:message% name)
	     (button-callback
	      (make-object mred:button% hp "" (lambda (b e) (button-callback b))))
	     (send body set-media edit)))])
      (sequence (super-init)))))
