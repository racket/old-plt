    (define unattached-nonsnip%
      (class null ()
	(private [pb-ht (make-hash-table-weak)])
	(public
	  [add-pb
	   (lambda (pb snip)
	     (hash-table-put! pb-ht pb snip))]
	  [get-snip
	   (lambda (pb)
	     (hash-table-get pb-ht pb))])

	(public
	  [insert-into
	   (lambda (buffer . args)
	     (let ([snip (make-object forward-snip% this)])
	       (apply (ivar buffer insert) snip args)
	       snip))]
	  [draw
	   (lambda (dc x y left top right bottom dx dy draw-caret)
	     (void))]
	  [get-extent
	   (lambda (dc x y w h descent space lspace rspace)
	     (let ([size (lambda (size)
			   (lambda (b)
			     (when (box? b)
			       (set-box! b size))))])
	       (for-each (size 20) (list w h))
	       (for-each (size 2) (list descent space lspace rspace))))]
	  [resize
	   (lambda (w h)
	     #f)]
	  [size-cache-invalid void]
	  [get-text
	   (opt-lambda (offset num [flattened? #f])
	     "unattached-nonsnip")])))

    (define unattached-media-nonsnip%
      (class unattached-nonsnip% (buffer)
	(public
	  [insert-into
	   (lambda (buffer . args)
	     (let ([snip (make-object forward-snip% this)])
	       (apply (ivar buffer insert) snip args)
	       snip))]
	  [draw
	   (lambda (dc x y left top right bottom dx dy draw-caret)
	     (void))]
	  [get-extent
	   (lambda (dc x y w h descent space lspace rspace)
	     (let ([size (lambda (size)
			   (lambda (b)
			     (when (box? b)
			       (set-box! b size))))])
	       (for-each (size 20) (list w h))
	       (for-each (size 2) (list descent space lspace rspace))))]
	  [resize
	   (lambda (w h)
	     #f)]
	  [size-cache-invalid void]
	  [get-text
	   (opt-lambda (offset num [flattened? #f])
	     "unattached-nonsnip")])))
	   
    (define forward-snipclass
      (make-object (class wx:snip-class% ()
		     (inherit set-classname set-version)
		     (public
		       [read void])
		     (sequence
		       (super-init)
		       (set-classname "forward-snipclass")
		       (set-version 1)))))

    (send (wx:get-the-snip-class-list) add forward-snipclass)
			
    (define (make-forward-pasteboard% pasteboard%)
      (class-asi pasteboard%
	(rename
	 [super-on-insert on-insert])
	(public
	  [on-insert
	   (lambda (snip before x y)
	     (send (send snip get-unattached) add-pb this snip))])))
		    

    (define forward-snip%
      (class wx:snip% (unattached)
	(inherit set-snipclass)
	(public
	  [get-unattached
	   (lambda ()
	     unattached)])
	(public
	  [copy
	   (lambda ()
	     (make-object forward-snip% unattached))]
	  [get-extent
	   (lambda (dc x y w h descent space lspace rspace)
	     (send unattached get-extent dc x y w h descent space lspace rspace))]
	  [draw
	   (lambda (dc x y left top right bottom dx dy draw-caret)
	     (send unattached draw dc x y left top right bottom dx dy draw-caret))]
	  [resize
	   (lambda (w h)
	     (send unattached resize w h))]
	  [size-cache-invalid 
	   (lambda ()
	     (send unattached size-cache-invalid))]
	  [get-text
	   (opt-lambda (offset num [flattened? #f])
	     (send unattached get-text offset num flattened?))])
	(sequence
	  (super-init)
	  (set-snipclass forward-snipclass))))
    
