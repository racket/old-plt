(require-library "sig.ss" "mred")

(define mred:startup
  (lambda args
    (let* ([orig-width 100]
	   [orig-height 100]
	   [frame (make-object wx:frame% null "mred:startup default" 
			       orig-width orig-height)]
	   [panel (make-object wx:panel% frame)]
	   [button (make-object wx:button% 
				panel 
				(lambda (button evt) (exit))
				"Quit")]
	   [bw (send button get-width)]
	   [bh (send button get-height)]
	   [w (box 0)]
	   [h (box 0)])
      (send button set-size 0 0 bw bh)
      (send frame set-size 0 0 orig-width orig-height)
      (send frame get-client-size w h)
      (let ([new-w (+ bw (- orig-width (unbox w)))]
	    [new-h (+ bh (- orig-height (unbox h)))])
	(send frame set-size 0 0 new-w new-h)
	(send panel set-size 0 0 new-w new-h))
      (send frame show #t))))
