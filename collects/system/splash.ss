(define mred:splash-frame #f)
(define mred:splash-message #f)

(define mred:close-splash
  (lambda ()
    (when mred:splash-frame
      (send mred:splash-frame show #f)
      (set! mred:splash-frame #f)
      (set! mred:splash-message #f))))

(define mred:open-splash
  (lambda (filename title)
    (if (file-exists? filename)
	(let* ([len (string-length filename)]
	       [flag (if (<= len 4)
			 wx:const-bitmap-type-default
			 (let ([suffix (substring filename (- len 4) len)])
			   (cond
			    [(string-ci=? ".xpm" suffix) wx:const-bitmap-type-xpm]
			    [(string-ci=? ".xbm" suffix) wx:const-bitmap-type-xbm]
			    [(string-ci=? ".gif" suffix) wx:const-bitmap-type-gif]
			    [(string-ci=? "pict" suffix) wx:const-bitmap-type-pict]
			    [else wx:const-bitmap-type-default])))]
	       [bitmap (make-object wx:bitmap% filename flag)])
	  (if (send bitmap ok?)
	      (let* ([frame (parameterize ([wx:current-eventspace (wx:make-eventspace)])
			      (make-object wx:dialog-box% '() title))]
		     [width (box 0.)]
		     [height (box 0.)]
		     [c-x-offset 0]
		     [c-y-offset 0]
		     [panel (make-object wx:panel% frame)]
		     [message (make-object wx:message% panel bitmap)]
		     [_ (send panel new-line)]
		     [text-message (make-object wx:message% panel (format "Welcome to ~a" title))]
		     [text-msg-height (send text-message get-height)]
		     [msg-width (send message get-width)]
		     [msg-height (send message get-height)])
		(wx:display-size width height)
		(send panel fit)
		(set! width (unbox width))
		(set! height (unbox height))
		(let ([frame-width (+ c-x-offset (send panel get-width))]
		      [frame-height (+ c-y-offset (send panel get-height))]
		      [frame-x (/ (- (+ width c-x-offset) msg-width) 2)]
		      [frame-y (/ (- (+ height c-y-offset) msg-height) 2)])
		  (send frame set-size 0 0 width height)
		  (let-values ([(c-x-offset c-y-offset)
				(let ([cwidth (box 0.)]
				      [cheight (box 0.)])
				  (send frame get-client-size cwidth cheight)
				  (values (- width (unbox cwidth))
					  (- height (unbox cheight))))])
		    (send frame set-size frame-x frame-y frame-width frame-height)
		    (send frame show #t)
		    (wx:flush-display) (wx:yield)
		    (set! mred:splash-message text-message)
		    (set! mred:splash-frame frame)
		    (current-load
		     (let ([old-load (current-load)])
		       (lambda (f)
			 (mred:change-splash-message (format "Loading ~a" f))
			 (old-load f)))))))
	      (printf "WARNING: bad bitmap ~s" filename)))
	(printf "WARNING: bitmap path ~s not found~n" filename))))

(define mred:change-splash-message
  (lambda (s)
    (when mred:splash-message
      (send mred:splash-message set-label s))))
