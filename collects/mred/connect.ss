  (unit/sig mred:connections^
    (import mred:wx^
	    [mred:constants : mred:constants^]
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
	(class % (parent x y width height name style scrolls-per-page init-buffer)
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

	  (inherit get-media is-focus-on?)
  
	  (private
	    [update-active-canvas
	     (lambda (m)
	       (when (and (not (null? m))
			  (or (not (ivar m active-canvas))
			      (is-focus-on?)))
		 (send m set-active-canvas this)))])
	  (public
	    [edit-renamed
	     (lambda (name)
	       (void))]
	    [edit-modified
	     (lambda (modified?)
	       (void))]
	    [set-media
	     (opt-lambda (media [redraw? #t])
	       (let ([m (get-media)])
		 (update-active-canvas media)
		 (unless (null? m)
		   (send m remove-canvas this)))
	       (super-set-media media redraw?)
	       (unless (null? media)
		 (send media add-canvas this)))]
	    [on-set-focus
	     (lambda ()
	       (mred:debug:printf 'matthew "connect-canvas::on-set-focus~n")
	       (when frame
		 (send frame active-canvas this)
		 (mred:debug:printf 'matthew "connect-canvas::on-set-focus.2~n")
		 (send frame on-frame-active)
		 (mred:debug:printf 'matthew "connect-canvas::on-set-focus.3~n"))
	       (let ([m (get-media)])
		 (unless (null? m)
		   (mred:debug:printf 'matthew "connect-canvas::on-set-focus.4~n")
		   (send m set-active-canvas this)
		   (mred:debug:printf 'matthew "connect-canvas::on-set-focus.5~n")))
	       (super-on-set-focus))])
	  (sequence
	    (super-init parent x y width height name style scrolls-per-page init-buffer)
	    (update-active-canvas init-buffer)))))

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
	  (rename [super-set-modified set-modified]
		  [super-set-filename set-filename])
	  (public
	    [canvases null]
            [set-filename
             (opt-lambda (name [temp? #f])
               (super-set-filename name temp?)
               (for-each (lambda (canvas) (send canvas edit-renamed name))
                         canvases))]
            [set-modified
             (lambda (modified?)
               (super-set-modified modified?)
               (for-each (lambda (canvas) (send canvas edit-modified modified?))
                         canvases))]
	    [get-canvas
	     (lambda ()
	       (or active-canvas
		   (and (not (null? canvases))
			(car canvases))))]
	    [get-frame
	     (lambda ()
	       (and active-canvas
		    (ivar active-canvas frame)))]
	    [active-canvas #f]
	    [set-active-canvas
	     (lambda (new-canvas)
	       (set! active-canvas new-canvas))]
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
    (define connections-media-canvas% (make-connections-media-canvas% wx:media-canvas%)))
