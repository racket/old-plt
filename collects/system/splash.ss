(define mred:splash
  (lambda (info)
    (let/ec k
      (let*-values
	  ([(no-splash) (lambda () (k void void void))]
	   [(get-resource)
	    (lambda (name default)
	      (let ([b (box 0)])
		(if (wx:get-resource "mred" name b null)
		    (unbox b)
		    default)))]
	   [(set-resource)
	    (lambda (name value)
	      (wx:write-resource "mred" name value (wx:find-path 'setup-file)))]
	   [(filename) (info 'splash-image-path (lambda () #f))]
	   [(_)
	    (begin
	      (unless filename
		(no-splash))
	      (unless (file-exists? filename)
		(fprintf (current-error-port) "WARNING: bitmap path ~s not found~n" filename)
		(no-splash)))]

	   [(title) (info 'name (lambda () "Splash"))]

	   [(splash-width-resource) (format "~a-splash-max-width" title)]
	   [(splash-depth-resource) (format "~a-splash-max-depth" title)]
	   [(splash-max-width) (get-resource splash-width-resource
					     (info 'splash-max
						   (lambda () 100)))]
	   [(splash-max-depth) (get-resource splash-depth-resource
					     (info 'splash-depth
						   (lambda () 5)))]
	   
	   [(splash-sofar-depth) 0]
	   [(splash-current-width) 0]

	   [(splitup-path)
	    (lambda (f)
	      (let*-values ([(absf) (if (relative-path? f)
					(build-path (current-directory) f)
					f)]
			    [(base name _1) (split-path absf)])

		(if base
		    (let-values ([(base2 name2 _2) (split-path base)])
		      (if base2
			  (let-values ([(base3 name3 _2) (split-path base2)])
			    (build-path name3 name2 name))
			  (build-path name2 name)))
		    name)))]

	   [(frame) (parameterize ([wx:current-eventspace (wx:make-eventspace)])
		      (make-object wx:dialog-box% '() title))]
	   [(bitmap-flag)
	    (let ([len (string-length filename)])
	      (if (<= len 4)
		  wx:const-bitmap-type-default
		  (let ([suffix (substring filename (- len 4) len)])
		    (cond
		     [(string-ci=? ".xpm" suffix) wx:const-bitmap-type-xpm]
		     [(string-ci=? ".xbm" suffix) wx:const-bitmap-type-xbm]
		     [(string-ci=? ".gif" suffix) wx:const-bitmap-type-gif]
		     [(string-ci=? "pict" suffix) wx:const-bitmap-type-pict]
		     [else wx:const-bitmap-type-default]))))]
	   [(bitmap) (make-object wx:bitmap% filename bitmap-flag)]
	   [(_) (unless (send bitmap ok?)
		  (fprintf (current-error-port) "WARNING: bad bitmap ~s" filename)
		  (no-splash))]
	   [(logo-memory-dc) (make-object wx:memory-dc%)]
	   [(_) (send logo-memory-dc select-object bitmap)]
	   [(canvas%)
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
	   [(box1) (box 0.)]
	   [(box2) (box 0.)]
	   [(c-x-offset) 0]
	   [(c-y-offset) 0]
	   [(panel) (make-object wx:panel% frame)]
	   
	   [(ready-cursor) (lambda () (send panel new-line))]
	   [(_) (send panel set-item-cursor 0 0)]
	   
	   [(logo-canvas) (make-object canvas% panel)]
	   [(_) (ready-cursor)]
	   [(splash-messages)
	    (cons (make-object wx:message% panel (format "Welcome to ~a" title))
		  (let loop ([n (- splash-max-depth 1)])
		    (cond
		     [(zero? n) null]
		     [else (cons (make-object wx:message% panel "")
				 (loop (sub1 n)))])))]
	   [(_) (ready-cursor)]
	   [(gauge) (make-object wx:gauge% panel null splash-max-width)]
	   [(text-msg-height) (send (car splash-messages) get-height)]
	   [(text-msg-width) (send (car splash-messages) get-width)]
	   [(logo-width logo-height)
	    (let ([bw (send bitmap get-width)]
		  [bh (send bitmap get-height)])
	      (send logo-canvas set-size 0 0 bw bh)
	      (send logo-canvas get-client-size box1 box2)
	      (let ([w (+ bw (- bw (unbox box1)))]
		    [h (+ bh (- bh (unbox box2)))])
		(send logo-canvas set-size 0 0 w h)
		(values w h)))]
	   [(gauge-height) (send gauge get-height)]
	   [(width height)
	    (begin
	      (wx:display-size box1 box2)
	      (values (unbox box1)
		      (unbox box2)))]
	   [(margin) 2]
	   [(indent) 5]
	   [(_) (begin
		  (send gauge set-size
			0
			(+ margin logo-height)
			logo-width
			gauge-height)
		  (let loop ([splash-messages splash-messages]
			     [count 0]
			     [last-bottom (+ (* 2 margin) logo-height gauge-height)])
		    (cond
		     [(null? splash-messages) (void)]
		     [else
		      (let ([text-message (car splash-messages)])
			(send text-message set-size
			      (* count indent) 
			      last-bottom
			      (- logo-width (* count indent))
			      text-msg-height)
			(loop (cdr splash-messages)
			      (+ 1 count)
			      (+ last-bottom text-msg-height)))]))
		  (send panel set-size 0 0
			logo-width
			(+ (* 2 margin) logo-height 
			   (* text-msg-height (length splash-messages))
			   gauge-height)))]
	   
	   [(_) (when (getenv "MREDDEBUG")
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
			       on-mdc off-mdc))))))))]
	   [(panel-width) (send panel get-width)]
	   [(panel-height) (send panel get-height)]
	   [(_) (send frame set-size 0 0 panel-width panel-height)]
	   [(c-x-offset c-y-offset)
	    (begin
	      (send frame get-client-size box1 box2)
	      (values (- panel-width (unbox box1))
		      (- panel-height (unbox box2))))]

	   [(frame-width) (+ c-x-offset panel-width)]
	   [(frame-height) (+ c-y-offset panel-height)]
	   [(_) (begin
		  (send frame set-size 0 0 frame-width frame-height)
		  (send frame center wx:const-both)
		  (send frame show #t)
		  (wx:flush-display) (wx:yield) (sleep)
		  (wx:flush-display) (wx:yield) (sleep))]
	   [(change-splash-message)
	    (letrec ([change-splash-message
		      (case-lambda 
		       [(s) (change-splash-message s 0 #f)]
		       [(s depth clear-after)
			(unless (null? splash-messages)
			  (if (< depth splash-max-depth)
			      (begin '(printf "setting depth ~a (of ~a) to ~s~n" depth splash-max-depth s)
				     (send (list-ref splash-messages depth) set-label s)
				     (when (and clear-after
						(< (+ depth 1) splash-max-depth))
				       (let ([next-message (list-ref splash-messages (+ depth 1))])
					 (unless (string=? "" (send next-message get-label))
					   '(printf "clearing depth ~a (of ~a)~n"
						    (+ depth 1) current-splash-max-depth)
					   (send next-message set-label ""))))
				     #t)
			      #f))])])
	      change-splash-message)]
	   [(splash-load-handler)
	    (let ([depth 0])
	      (lambda (old-load f)
		(let ([finalf (splitup-path f)])
		  (set! splash-sofar-depth (max (+ depth 1) splash-sofar-depth))
		  (set! splash-current-width (+ splash-current-width 1))
		  (when (change-splash-message (format "Loading ~a..." finalf) depth #f)
		    (when (<= splash-current-width splash-max-width)
		      (send gauge set-value splash-current-width)))
		  (set! depth (+ depth 1))
		  (begin0
		   (old-load f)
		   (begin (set! depth (- depth 1))
			  (change-splash-message (format "Loading ~a...done." finalf)
						 depth #t))))))]
	   [(_) (current-load
		 (let ([old-load (current-load)])
		   (lambda (f)
		     (splash-load-handler old-load f))))]
	   [(shutdown-splash)
	    (lambda ()
	      (set! splash-load-handler (lambda (old-load f) (old-load f)))
	      (unless (= splash-max-width splash-current-width)
		(set-resource splash-width-resource splash-current-width))
	      (unless (= splash-max-depth splash-sofar-depth)
		(set-resource splash-depth-resource splash-sofar-depth)))]
	   [(close-splash)
	    (lambda ()
	      (send frame show #f))])
	(values
	 change-splash-message
	 shutdown-splash
	 close-splash)))))
