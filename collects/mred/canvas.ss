  (unit/sig mred:canvas^
    (import [wx : wx^]
	    [mred:constants : mred:constants^]
	    [mred:container : mred:container^]
	    [mred:edit : mred:edit^]
	    [mred:preferences : mred:preferences^]
	    [mzlib:file : mzlib:file^]
	    [mzlib:function : mzlib:function^])
	    
    (mred:debug:printf 'invoke "mred:canvas@")

    (define-struct range (start end b/w-bitmap color))
    (define-struct rectangle (left top width height b/w-bitmap color))

    (define make-wrapping-canvas%
      (lambda (super%)
	(class super% (parent [x -1] [y -1] [w -1] [h -1] 
			      [name ""] [style 0] [spp 100] [m ()])
	  (sequence (mred:debug:printf 'creation "creating a canvas"))
	  (public
	    [get-edit% (lambda () mred:edit:edit%)]
	    [style-flags 0])
	  (inherit get-media get-parent force-redraw)
	  (public
	    [get-style-flags (lambda () style-flags)]
	    [make-edit (lambda () (make-object (get-edit%)))]
	    [make-initial-edit (lambda () null)])

	  (private
	    [rewrap
	     (lambda ()
	       (let ([edit (get-media)])
		 (unless (null? edit)
		   (mred:debug:printf 'rewrap "canvas:rewrap")
		   (send edit rewrap))))])
	  (public
	    [resize-edit
	     (lambda ()
	       (force-redraw))]
	    [on-container-resize
	     (lambda ()
	       (rewrap))])

	  (rename
	    [super-set-media set-media])

	  (public [edit-modified void]
		  [edit-renamed void])

	  (public
	    [set-media
	     (opt-lambda (media [redraw? #t])
	       (super-set-media media redraw?)
	       (edit-modified (and (not (null? media))
				   (send media modified?)))
	       (edit-renamed (if (null? media)
				 "No buffer"
				 (send media get-filename)))
	       (mred:debug:printf 'rewrap "rewrap: setting the media: ~a" media)
	       (unless (null? media)
		 (mred:debug:printf 'rewrap "rewrap: setting the media")
		 (resize-edit)))])
	  (sequence
	    (super-init parent x y w h
			name (bitwise-ior style (get-style-flags))
			spp
			(if (null? m)
			    (make-initial-edit)
			    m))))))

    (define make-frame-title-canvas%
      (lambda (super%)
	(class super% args
	  (inherit get-media edit-modified get-parent frame)
	  (rename
	    [super-edit-renamed edit-renamed]
	    [super-set-media set-media]
	    [super-on-set-focus on-set-focus])
	  (private
	    [set-frame-title
	     (lambda ()
	       (when frame
		 (mred:debug:printf 'matthew "set-frame-title.1~n")
		 (let* ([edit (get-media)]
			[_ (mred:debug:printf 'matthew "set-frame-title.2~n")]
			[title 
			 (if (null? edit)
			     ""
			     (let ([fullname (send edit get-filename)])
			       (if (null? fullname)
				   ""
				   (trim-filename fullname))))])
		   (mred:debug:printf 'matthew "set-frame-title.3~n")
		   (send frame set-title title))))])
	  (public
	    [trim-filename
	     (lambda (fullname)
	       (or (mzlib:file:file-name-from-path fullname)
		   "Untitled"))]
	    [edit-renamed
	     (lambda (name)
	       (super-edit-renamed name)
	       (set-frame-title))]
	    [on-set-focus
	     (lambda ()
	       (mred:debug:printf 'matthew "canvas:: on-set-focus~n")
	       (set-frame-title)
	       (mred:debug:printf 'matthew "canvas::super-on-set-focus~n")
	       (super-on-set-focus)	       
	       (mred:debug:printf 'matthew "end canvas::on-set-focus~n"))])
	  (public
	    [set-media
	     (opt-lambda (m [redraw? #t])
	       (super-set-media m redraw?))])
	  (sequence
	    (apply super-init args)))))

    (define make-one-line-canvas%
      (lambda (super%)
	(class super% (parent [x -1] [y -1] [w -1] [h -1] 
			      [name ""] [style 0] [spp 100] [m ()]
			      [lnes 1])
	  
	  (inherit get-media call-as-primary-owner user-min-height
		   get-size set-min-height)
	  (rename [super-set-media set-media])
	  (public 
	    [lines lnes]
	    [set-lines (lambda (x) 
			 (set! lines x)
			 (update-size (get-media)))])
	  (private
	    [update-size
	     (lambda (media)
	       (unless (null? media)
		 (let* ([top (send media line-location 0 #t)]
			[bottom (send media line-location 0 #f)]
			[height (- bottom top)])
		   (let* ([ch (box 0)]
			  [h (box 0)])
		     (call-as-primary-owner
		      (lambda ()
			(send (send media get-admin) 
			      get-view null null null ch)))
		     (get-size (box 0) h)
		     (let ([new-min-height (+ (* lines height) 
					      (- (unbox h) (unbox ch)))])
		       (set-min-height new-min-height)
		       (user-min-height new-min-height))))))])
	  (public
	    [style-flags (bitwise-ior wx:const-mcanvas-hide-h-scroll
				      wx:const-mcanvas-hide-v-scroll)]
	    [default-y-stretch #f]
	    [set-media
	     (lambda (media)
	       (super-set-media media)
	       (update-size media))])
	  (sequence
	    (super-init parent x y w h name style spp m)
	    (update-size (get-media))))))

    (define make-wide-snip-canvas%
      (lambda (super%)
	(class-asi super%
	  (inherit get-media)
	  (rename [super-on-size on-size])
	  (private
	    [wide-snips null]
	    [tall-snips null]
	    [autowrap-snips? (mred:preferences:get-preference 'mred:auto-set-wrap?)]
	    [update-snip-size
	     (lambda (width?)
	       (lambda (s)
		 (let* ([width (box 0)]
			[height (box 0)]
			[leftm (box 0)]
			[rightm (box 0)]
			[topm (box 0)]
			[bottomm (box 0)]
			[left-edge-box (box 0)]
			[top-edge-box (box 0)]
			[snip-media (send s get-this-media)]
			[edit (get-media)])
		   (unless (null? edit)
		     (send edit
			   run-after-edit-sequence
			   (lambda ()
			     (let ([admin (send edit get-admin)])
			       (send admin get-view null null width height)
			       (send s get-margin leftm topm rightm bottomm)


			       ;; when the width is to be maximized and there is a
			       ;; newline just behind the snip, we know that the left
			       ;; edge is zero. Special case for efficiency in the 
			       ;; console printer
			       (let ([fallback
				      (lambda ()
					(send edit get-snip-position-and-location
					      s null left-edge-box top-edge-box))])
				 (cond
				   [(not width?) (fallback)]
				   [(let ([prev (send s previous)])
				      (and (not (null? prev))
					   (not (zero? (bitwise-and wx:const-snip-hard-newline
								    (send prev get-flags))))))
				    (set-box! left-edge-box 0)]
				   [else (fallback)]))


			       (if width?
				   (let ([snip-width (- (unbox width)
							(unbox left-edge-box)
							(unbox leftm)
							(unbox rightm)
							
							;; this two is the space that 
							;; the caret needs at the right of
							;; a buffer.
							2)])
				     (send* s 
				       (set-min-width snip-width)
				       (set-max-width snip-width))
				     (unless (null? snip-media)
				       (send snip-media set-max-width
					     (if autowrap-snips?
						 snip-width
						 0))))
				   (let ([snip-height (- (unbox height)
							 (unbox top-edge-box)
							 (unbox topm)
							 (unbox bottomm))])
				     (send* s 
				       (set-min-height snip-height)
				       (set-max-height snip-height)))))))))))])
	  (public
	    [widen-snips
	     (lambda (x)
	       (set! autowrap-snips? x)
	       (for-each (update-snip-size #t) wide-snips))]
	    [add-wide-snip
	     (lambda (snip)
	       (set! wide-snips (cons snip wide-snips))
	       ((update-snip-size #t) snip))]
	    [add-tall-snip
	     (lambda (snip)
	       (set! tall-snips (cons snip tall-snips))
	       ((update-snip-size #f) snip))]
	    [on-size
	     (lambda (width height)
	       (super-on-size width height)
	       (for-each (update-snip-size #t) wide-snips)
	       (for-each (update-snip-size #f) tall-snips))]))))

    (define wrapping-canvas% (make-wrapping-canvas% mred:container:media-canvas%))
    (define frame-title-canvas% (make-frame-title-canvas% wrapping-canvas%))
    (define one-line-canvas% (make-one-line-canvas% wrapping-canvas%))
    (define wide-snip-canvas% (make-wide-snip-canvas% wrapping-canvas%))
    
    (define number-control%
      (class one-line-canvas% args
	(private
	  [number 0]
	  [edit%
	   (class-asi mred:edit:edit%
	     (inherit get-text delete)
	     (rename [super-after-insert after-insert])
	     (public
	       [after-insert
		(lambda (start len)
		  (super-after-insert start len)
		  (let ([s (string->number (get-text))])
		    (if s 
			(set! number s)
			(delete start (+ start len)))))]))])
	(public
	  [get-number (lambda () number)]
	  [set-number (lambda (n) 
			(when (number? n)
			  (set! number n)
			  (send edit clear)
			  (send edit insert (number->string n))))])
	(sequence
	  (apply super-init args))
	(private
	  [edit (make-object edit%)])
	(inherit set-media)
	(sequence (send edit insert (number->string number))
		  (set-media edit)))))