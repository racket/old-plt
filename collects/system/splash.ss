(define mred:splash-frame #f)
(define mred:splash-messages null)

(define mred:splash-depth 4)
(define mred:splash-depth-counter 0)
(define mred:splash-max 93)
(define mred:splash-counter 0)
(define mred:splash-current-depth 0)

(define mred:close-splash
  (lambda ()
    (when mred:splash-frame
      (send mred:splash-frame show #f)
      (set! mred:splash-frame #f)
      (set! mred:splash-messages null)
      (unless (= mred:splash-max mred:splash-counter)
	(fprintf (current-error-port)
		 "splash; expected count: ~a actual count: ~a~n" 
		 mred:splash-max mred:splash-counter))
      (unless (= mred:splash-depth mred:splash-depth-counter)
	(fprintf (current-error-port)
		 "splash; expected depth: ~a actual depth: ~a~n"
		 mred:splash-depth mred:splash-depth-counter)))))

(define-values (mred:no-more-splash-messages mred:open-splash)
  (let ([clear-state void]
	[splitup-path
	 (lambda (f)
	   (let*-values ([(absf) (if (relative-path? f)
				     (build-path (current-directory) f)
				     f)]
			 [(base name _1) (split-path absf)])

	     (if base
		 (let-values ([(base2 name2 _2) (split-path base)])
		   (build-path name2 name))
		 name)))])
    (values 
     (lambda ()
       (set! mred:splash-messages null)
       (clear-state))
     (lambda (info)
       (let ([filename (info 'splash-image-path)])
	 (when filename
	   (let* ([title (info 'name)]
		  [splash-max (info 'splash-max)]
		  [splash-depth (info 'splash-depth)])
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
		       (let* ([logo-memory-dc (make-object wx:memory-dc%)]
			      [_ (send logo-memory-dc select-object bitmap)]
			      [canvas%
			       (class wx:canvas% args
				 (inherit get-dc)
				 (public
				   [on-paint
				    (lambda ()
				      (send (get-dc) blit 0 0 
					    (send bitmap get-width)
					    (send bitmap get-height)
					    logo-memory-dc
					    0 0))])
				 (sequence
				   (apply super-init args)))]
			      [frame (parameterize ([wx:current-eventspace (wx:make-eventspace)])
				       (make-object wx:dialog-box% '() title))]
			      [width (box 0.)]
			      [height (box 0.)]
			      [c-x-offset 0]
			      [c-y-offset 0]
			      [_ (set! mred:splash-max splash-max)]
			      [_ (set! mred:splash-depth splash-depth)]
			      [panel (make-object wx:panel% frame)]
			      
			      [ready-cursor
			       (let ([bx (box 0.)]
				     [by (box 0.)])
				 (lambda ()
				   (send panel new-line)
				   '(send panel get-item-cursor bx by)
				   '(send panel set-item-cursor 0 (unbox by))))]
			      [_ (send panel set-item-cursor 0 0)]
			      
			      [logo-canvas (make-object canvas% panel)]
			      [_ (ready-cursor)]
			      [text-messages (cons (make-object wx:message% panel (format "Welcome to ~a" title))
						   (let loop ([n (- splash-depth 1)])
						     (cond
						      [(zero? n) null]
						      [else (cons (make-object wx:message% panel "")
								  (loop (sub1 n)))])))]
			      [_ (ready-cursor)]
			      [gauge (make-object wx:gauge% panel null mred:splash-max)]
			      [text-msg-height (send (car text-messages) get-height)]
			      [text-msg-width (send (car text-messages) get-width)]
			      [logo-width 0]
			      [logo-height 0]
			      [_ (let ([wb (box 0)]
				       [hb (box 0)]
				       [bw (send bitmap get-width)]
				       [bh (send bitmap get-height)])
				   (send logo-canvas set-size 0 0 bw bh)
				   (send logo-canvas get-client-size wb hb)
				   (let ([w (+ bw (- bw (unbox wb)))]
					 [h (+ bh (- bh (unbox hb)))])
				     (send logo-canvas set-size 0 0 w h)
				     (set! logo-width w)
				     (set! logo-height h)))]
			      [gauge-height (send gauge get-height)])

			 (set! clear-state (lambda () (set! gauge #f)))

			 (wx:display-size width height)
			 (set! width (unbox width))
			 (set! height (unbox height))
			 (let ([bx (box 0.)]
			       [by (box 0.)]
			       [bwidth (box 0.)]
			       [bheight (box 0.)]
			       [margin 2])
			   (send gauge set-size
				 0 (+ margin logo-height)
				 logo-width gauge-height)
			   (let ([indent 5])
			     (let loop ([text-messages text-messages]
					[count 0]
					[last-bottom (+ (* 2 margin) logo-height gauge-height)])
			       (cond
				[(null? text-messages) (void)]
				[else
				 (let ([text-message (car text-messages)])
				   (send text-message set-size
					 (* count indent) 
					 last-bottom
					 (- logo-width (* count indent))
					 text-msg-height)
				   (loop (cdr text-messages)
					 (+ 1 count)
					 (+ last-bottom text-msg-height)))])))
			   (send gauge get-position bx by)
			   (send gauge get-size bwidth bheight)
			   (send panel set-size 0 0
				 logo-width
				 (+ (* 2 margin) logo-height 
				    (* text-msg-height (length text-messages))
				    gauge-height)))
			 
			 (when (getenv "MREDDEBUG")
			   (let-values ([(base _1 _2) (split-path filename)])
			     (let ([mars (build-path base "recycle.gif")])
			       (when (file-exists? mars)
				 (let* ([img-mdc (make-object wx:memory-dc%)]
					[img-bitmap (make-object wx:bitmap% mars wx:const-bitmap-type-gif)]
					[bw (send img-bitmap get-width)]
					[bh (send img-bitmap get-height)])
				   (when (and (< bw logo-width)
					      (< bh logo-height))
				     (let* ([on-mdc (make-object wx:memory-dc%)]
					    [on-bitmap (make-object wx:bitmap% bw bh)]
					    [off-mdc (make-object wx:memory-dc%)]
					    [off-bitmap (make-object wx:bitmap% bw bh)]
					    [lw (send bitmap get-width)]
					    [lh (send bitmap get-height)]
					    [bx (/ (- lw bw) 2)]
					    [by (/ (- lh bw) 2)])
				       (send on-mdc select-object on-bitmap)
				       (send off-mdc select-object off-bitmap)
				       (send img-mdc select-object img-bitmap)
				       (send off-mdc clear)
				       (send off-mdc blit 0 0 bw bh logo-memory-dc bx by)
				       (send on-mdc clear)
				       (send on-mdc blit 0 0 bw bh logo-memory-dc bx by)
				       (time 
					(let* ([color (make-object wx:colour% "WHITE")]
					       [red (ivar color red)]
					       [green (ivar color green)]
					       [blue (ivar color blue)]
					       [get-pixel (ivar img-mdc get-pixel)]
					       [set-pen (ivar on-mdc set-pen)]
					       [draw-point (ivar on-mdc draw-point)]
					       [find-or-create-pen (ivar wx:the-pen-list find-or-create-pen)])
					  (let loop ([x bw]
						     [y bh])
					    (get-pixel x y color)
					    (unless (= 255 (red) (green) (blue))
					      (set-pen (find-or-create-pen color 1 wx:const-solid))
					      (draw-point x y))
					    (cond
					     [(= x y 0) (void)]
					     [(= x 0) (loop bw (sub1 y))]
					     [else (loop (sub1 x) y)]))))
				       (wx:register-collecting-blit 
					logo-canvas 
					bx by bw bh
					on-mdc off-mdc))))))))

			 (let* ([panel-width (send panel get-width)]
				[panel-height (send panel get-height)])
			   (send frame set-size 0 0 panel-width panel-height)
			   (let-values ([(c-x-offset c-y-offset)
					 (let ([cwidth (box 0.)]
					       [cheight (box 0.)])
					   (send frame get-client-size cwidth cheight)
					   (values (- panel-width (unbox cwidth))
						   (- panel-height (unbox cheight))))])
			     
			     ;; center by the client width here
			     (let ([frame-width (+ c-x-offset panel-width)]
				   [frame-height (+ c-y-offset panel-height)])
			       (send frame set-size 0 0 frame-width frame-height))
			     (send frame center wx:const-both)
			     (send frame show #t)
			     (wx:flush-display) (wx:yield) (sleep)
			     (wx:flush-display) (wx:yield) (sleep)
			     (set! mred:splash-messages text-messages)
			     (set! mred:splash-frame frame)
			     (current-load
			      (let ([old-load (current-load)]
				    [depth 0])
				(lambda (f)
				  (if gauge
				      (let ([finalf (splitup-path f)])
					(set! mred:splash-counter (add1 mred:splash-counter))
					(when (mred:change-splash-message (format "Loading ~a..." finalf) depth #f)
					  (when (<= mred:splash-counter mred:splash-max)
					    (send gauge set-value mred:splash-counter)))
					(set! depth (+ depth 1))
					(begin0
					 (old-load f)
					 (begin (set! depth (- depth 1))
						(mred:change-splash-message (format "Loading ~a...done." finalf) depth #t))))
				      (old-load f))))))))
		       (begin (fprintf (current-error-port) "WARNING: bad bitmap ~s" filename)
			      (mred:no-more-splash-messages))))
		 (begin
		   (fprintf (current-error-port) "WARNING: bitmap path ~s not found~n" filename)
		   (mred:no-more-splash-messages))))))))))

(define mred:change-splash-message
  (case-lambda 
   [(s) (mred:change-splash-message s 0 #f)]
   [(s depth clear-after)
    (set! mred:splash-depth-counter (max (+ depth 1) mred:splash-depth-counter))
    (unless (null? mred:splash-messages)
      (set! mred:splash-current-depth depth)
      (if (< depth mred:splash-depth)
	  (begin '(printf "setting depth ~a (of ~a) to ~s~n" depth mred:splash-depth s)
		 (send (list-ref mred:splash-messages depth) set-label s)
		 (when (and clear-after
			    (< (+ depth 1) mred:splash-depth))
		   (let ([next-message (list-ref mred:splash-messages (+ depth 1))])
		     (unless (string=? "" (send next-message get-label))
		       '(printf "clearing depth ~a (of ~a)~n" (+ depth 1) mred:splash-depth)
		       (send next-message set-label ""))))
		 #t)
	  #f))]))
