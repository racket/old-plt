(define make-bitmap
  (let ([font (send wx:the-font-list find-or-create-font
		    12
		    wx:const-default
		    wx:const-normal
		    wx:const-normal)])
    (lambda (filename text)
      (let* ([img-bitmap
	      (make-object wx:bitmap% filename wx:const-bitmap-type-bmp)]
	     [img-width (send img-bitmap get-width)]
	     [img-height (send img-bitmap get-height)]
	     [img-memory-dc (make-object wx:memory-dc%)]
	     [width (box 0.)]
	     [height (box 0.)]
	     [descent (box 0.)]
	     [leading (box 0.)])
	(send img-memory-dc select-object img-bitmap)
	(send img-memory-dc get-text-extent text width height descent leading font)
	(let* ([margin 2]
	       [new-width (+ margin
			     img-width
			     margin
			     (unbox width)
			     margin)]
	       [new-height (+ margin
			      (max img-height
				   (+ (unbox height)
				      (unbox descent)
				      (unbox leading)))
			      margin)]
	       [memory-dc (make-object wx:memory-dc%)]
	       [new-bitmap (make-object wx:bitmap% new-width new-height -1)])
	  (send memory-dc select-object new-bitmap)
	  (send memory-dc set-font font)
	  (send memory-dc clear) 
	  (send memory-dc set-font font)
	  (send memory-dc draw-text text (+ margin img-width margin)
		(- (/ new-height 2) (/ (unbox height) 2)))
	  (send memory-dc blit
		margin
		(- (/ new-height 2) (/ img-height 2))
		img-width img-height
		img-memory-dc 0 0 wx:const-copy)
	  (send memory-dc select-object null)
	  new-bitmap)))))

(define test-frame
  (let* ([frame (make-object mred:frame% '() "")]
	 [panel (make-object mred:horizontal-panel% frame)]
	 [buttons
	  (map (lambda (filename)
		 (let* ([path (build-absolute-path
			       "/home" "robby" "plt" "drscheme"
			       (string-append filename ".bmp"))]
			[bitmap (make-bitmap path filename)])
		   (make-object mred:button% panel void bitmap)))
	       (list "go" "help" "save" "stop"))])
    (send frame show #t)
    frame))
      