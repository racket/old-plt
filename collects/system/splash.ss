(define mred:splash-frame #f)
(define mred:splash-message #f)

(define mred:splash-max 96)
(define mred:splash-counter 0)

(define mred:close-splash
  (lambda ()
    (when mred:splash-frame
      (send mred:splash-frame show #f)
      (set! mred:splash-frame #f)
      (set! mred:splash-message #f))))

(define-values (mred:no-more-splash-messages mred:open-splash)
  ;; thanks for this binding Richard!!
  (let ([WX-BORDER-SIZE (case wx:window-system
			  [(xt)  2]  ; grrrr....
			  [(motif) 0]
			  [(windows) 0]
			  [(macintosh) 0])]
	[clear-state void])
    (values 
     (lambda ()
       (set! mred:splash-message #f)
       (clear-state))
     (lambda (filename title splash-max)
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
			[_ (when splash-max
			     (set! mred:splash-max (read (open-input-string splash-max))))]
			[panel (make-object wx:panel% frame)]
			
			[ready-cursor
			 (let ([bx (box 0.)]
			       [by (box 0.)])
			   (lambda ()
			     (send panel new-line)
			     '(send panel get-item-cursor bx by)
			     '(send panel set-item-cursor 0 (unbox by))))]
			[_ (send panel set-item-cursor 0 0)]
			
			[message (make-object wx:message% panel bitmap)]
			[_ (ready-cursor)]
			[text-message (make-object wx:message% panel (format "Welcome to ~a" title))]
			[_ (ready-cursor)]
			[gauge (make-object wx:gauge% panel null mred:splash-max)]
			[text-msg-height (send text-message get-height)]
			[text-msg-width (send text-message get-width)]
			[msg-width (send message get-width)]
			[msg-height (send message get-height)]
			[gauge-height (send gauge get-height)])

		   (set! clear-state (lambda () (set! gauge (void))))

		   (wx:display-size width height)
		   (set! width (unbox width))
		   (set! height (unbox height))
		   (let ([bx (box 0.)]
			 [by (box 0.)]
			 [bwidth (box 0.)]
			 [bheight (box 0.)]
			 [margin 2])
		     (send message set-size 0 0 msg-width msg-height)
		     (send text-message set-size
			   0 (+ margin msg-height)
			   msg-width
			   text-msg-height)
		     (send gauge set-size
			   0 (+ (* 2 margin) msg-height text-msg-height)
			   msg-width gauge-height)
		     (send gauge get-position bx by)
		     (send gauge get-size bwidth bheight)
		     (send panel set-size 0 0
			   msg-width
			   (+ (* 2 margin) msg-height text-msg-height gauge-height)))
		   
		   (let* ([panel-width (send panel get-width)]
			  [panel-height (send panel get-height)])
		     (send frame set-size 0 0 panel-width panel-height)
		     (let-values ([(c-x-offset c-y-offset)
				   (let ([cwidth (box 0.)]
					 [cheight (box 0.)])
				     (send frame get-client-size cwidth cheight)
				     (values (+ (* 2 WX-BORDER-SIZE)
						(- panel-width (unbox cwidth)))
					     (+ (* 2 WX-BORDER-SIZE)
						(- panel-height (unbox cheight)))))])
		       
		       ;; center by the client width here
		       (let ([frame-width (+ c-x-offset panel-width)]
			     [frame-height (+ c-y-offset panel-height)])
			 (send frame set-size 0 0 frame-width frame-height))
		       (send frame center wx:const-both)
		       (send frame show #t)
		       (wx:flush-display) (wx:yield)
		       (set! mred:splash-message text-message)
		       (set! mred:splash-frame frame)
		       (current-load
			(let ([old-load (current-load)])
			  (lambda (f)
			    (when (mred:change-splash-message (format "Loading ~a..." f))
			      (set! mred:splash-counter (add1 mred:splash-counter))
			      (when (<= mred:splash-counter mred:splash-max)
				(send gauge set-value mred:splash-counter)))
			    (old-load f)))))))
		 (begin (printf "WARNING: bad bitmap ~s" filename)
			(mred:no-more-splash-messages))))
	   (begin
	     (printf "WARNING: bitmap path ~s not found~n" filename)
	     (mred:no-more-splash-messages)))))))

(define mred:change-splash-message
  (lambda (s)
    (if mred:splash-message
	(begin (send mred:splash-message set-label s)
	       #t)
	#f)))
