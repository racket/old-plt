(define mred:connections@
  (unit/sig mred:connections^
    (import [mred:debug : mred:debug^]
	    [mzlib:function : mzlib:function^])

    (mred:debug:printf 'invoke "mred:connections@")

    (define make-connections-frame%
      (lambda (%)
	(class-asi %
	  (rename [super-show show])
	  (public
	    [shown #f]
	    [show (lambda (x) 
		    (set! shown x)
		    (super-show x))]
	    [on-frame-active void]
	    [active-edit
	     (lambda ()
	       (let ([c (active-canvas)])
		 (and c
		      (send c get-media))))]
	    [active-canvas
	     (let ([current-active #f])
	       (case-lambda
		[() current-active]
		[(new-active) (set! current-active new-active)]))]))))

    (define find-my-frame
      (lambda (parent)
	(let loop ([obj parent])
	  (cond
	    [(or (is-a? obj wx:frame%)
		 (is-a? obj wx:dialog-box%))
	      obj]
	    [(null? obj) #f]
	    [else (loop (send obj get-parent))]))))

    (define make-connections-media-canvas%
      (lambda (%)
	(class % (parent . args)
	  (rename [super-set-media set-media]
		  [super-on-set-focus on-set-focus])
	  (public
	    [frame (find-my-frame parent)])

	  (rename [super-show show])
	  (public
	    [shown #f]
	    [show (lambda (x)
		    (set! shown x)
		    (super-show x))])

	  (inherit get-media)
  
	  (public
	    [set-media
	     (opt-lambda (media [redraw? #t])
	       (let ([m (get-media)])
		 (unless (null? m)
		   (send m remove-canvas this)))
	       (super-set-media media redraw?)
	       (unless (null? media)
		 (send media add-canvas this)))]
	    [on-set-focus
	     (lambda ()
	       (when frame
		 (send frame active-canvas this)
		 (send frame on-frame-active))
	       (let ([m (get-media)])
		 (unless (null? m)
		   (send m set-active-canvas this)))
	       (super-on-set-focus))])
	  (sequence
	    (apply super-init parent args)))))

    (define make-connections-panel%
      (lambda (%)
	(class % (parent . args)
	  (public
	    [frame (find-my-frame parent)])
	  (sequence
	    (apply super-init parent args)))))

    (define make-connections-media-buffer%
      (lambda (%)
	(class-asi %
	  (public
	    [get-canvas
	     (lambda ()
	       (or active-canvas
		   (and (not (null? canvases))
			(car canvases))))]
	    [get-frame
	     (lambda ()
	       (let ([c active-canvas])
		 (and c
		      (ivar c frame))))]
	    [active-canvas #f]
	    [set-active-canvas
	     (lambda (new-canvas)
	       (set! active-canvas new-canvas))]
	    [canvases null]
	    [add-canvas
	     (lambda (new-canvas)
	       (unless (member new-canvas canvases)
		 (set! canvases (cons new-canvas canvases))))]
	    [remove-canvas
	     (lambda (old-canvas)
	       (when (eq? old-canvas active-canvas)
		 (set! active-canvas #f))
	       (set! canvases (mzlib:function:remq old-canvas canvases)))]))))

    (define connections-panel% (make-connections-panel% wx:panel%))
    (define connections-frame% (make-connections-frame% wx:frame%))
    (define connections-dialog-box% (make-connections-frame% wx:dialog-box%))
    (define connections-media-edit% (make-connections-media-buffer% wx:media-edit%)) 
    (define connections-media-pasteboard% (make-connections-media-buffer% wx:media-pasteboard%))
    (define connections-media-canvas% (make-connections-media-canvas% wx:media-canvas%))))
