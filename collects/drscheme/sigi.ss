(define bundleI
  (interface () insert-tree))

(define bundle-namesI
  (interface (bundleI) get-names))

(define bundle-joinI
  (interface (bundleI) get-children))

(define bundle-names%
  (class* unattached-nonsnip% (bundle-namesI) names
    (inherit insert-into)
    (private
      [width 10]
      [height 10])
    (public
      [get-width (lambda () width)]
      [get-tree-width (lambda () width)]
      [get-height (lambda () height)]
      [get-tree-height (lambda () height)]
      [resize
       (lambda (w h)
	 (set! width w)
	 (set! height h))])

    (public
      [insert-tree void]
      [insert-position
       (lambda (pb x y)
	 (let ([snip (insert-into pb)])
	   (send pb move-to snip x y)
	   x))])

    (public
      [get-names
       (lambda ()
	 names)])
    (public
      [draw
       (lambda (dc x y left top right bottom dx dy draw-caret)
	 (send dc draw-rectangle x y width height))])
    (sequence
      (super-init))))

(define bundle-join%
  (class* unattached-nonsnip% (bundle-joinI) children
    (private
      [width 10]
      [height 10])
    (inherit insert-into)

    (private
      [vspace 5]
      [hspace 5])

    (public
      [get-width (lambda () width)]
      [get-height (lambda () height)]
      [resize
       (lambda (w h)
	 (set! width w)
	 (set! height h))])

    (public
      [get-tree-width
       (lambda ()
	 (foldl (lambda (child width) (+ width
					 vspace
					 (send child get-tree-width)))
		0
		children))]
      [get-tree-height
       (lambda ()
	 (foldl (lambda (child height)
		  (max
		   height
		   (+ hspace (send child get-tree-height))))
		0
		children))]
      [insert-tree
       (lambda (pb)
	 (let ([tw (get-tree-width)]
	       [th (get-tree-height)]
	       [admin (send pb get-admin)]
	       [wb (box 0)]
	       [hb (box 0)])
	   (send admin get-view null null wb hb)
	   (let ([w (+ 500 (unbox wb))]
		 [h (+ 500 (unbox hb))])
	     (printf "tw: ~a th: ~a w: ~a h: ~a~n" tw th w h)
	     (insert-position pb
			      (- (/ w 2) (/ tw 2))
			      (- (/ h 2) (/ th 2))))))]
      [insert-position
       (lambda (pb x y)
	 (let ([snip (insert-into pb)]
	       [child-y (+ y height vspace)]
	       [bx (box 0)])
	   (let loop ([children children]
		      [last-x-pos x]
		      [x-off 0])
	     (cond
	      [(null? children)
	       (let* ([inserted-x last-x-pos]
		      [inserted-x x]
		      [inserted-x (/ (+ x last-x-pos) 2)]
		      [inserted-x (+ x (/ x-off 2))])
		 (send pb move-to snip inserted-x y)
		 inserted-x)]
	      [else (let ([child (car children)]
			  [x-pos (+ x x-off)])
		      (loop (cdr children)
			    (send child insert-position pb x-pos child-y)
			    (+ x-off (send child get-tree-width) hspace)))]))))]
      [get-children
       (lambda ()
	 children)])
    (public
      [draw
       (lambda (dc x y left top right bottom dx dy draw-caret)
	 (send dc draw-ellipse x y width height))])

    (sequence
      (super-init))))

(define bundle-pasteboard%
  (class-asi (make-forward-pasteboard% mred:pasteboard%)
    (inherit get-snip-location find-first-snip)
    (private
      [tree #f])
    (public
      [set-tree
       (lambda (t)
	 (send t insert-tree this)
	 (set! tree t))]
      [on-paint
       (lambda (before dc left top right bottom dx dy draw-caret)
	 (when (and before tree)
	   (let ([bx (box 0)]
		 [by (box 0)])
	     (let loop ([snip (find-first-snip)])
	       (cond
		[(null? snip) (void)]
		[else
		 (let ([unattached (send snip get-unattached)])
		   (when (is-a? unattached bundle-join%)
		     (get-snip-location snip bx by)
		     (let ([x (unbox bx)]
			   [y (unbox by)])
		       (for-each (lambda (child)
				   (get-snip-location (send child get-snip this) bx by)
				   (let ([cx (unbox bx)]
					 [cy (unbox by)])
				     (send dc draw-line x y cx cy)))
				 (send unattached get-children)))))
		 (loop (send snip next))])))))])))
