;; code to maintain the table
(define roots null)
(define ht (make-hash-table))
(define add-relation 
  (lambda (child parent)
    (unless (symbol? child)
      (error 'add-relation "expected symbol for child, got: ~s" child))
    (if parent
	(let ([l (hash-table-get ht parent (lambda () null))])
	  (unless (symbol? parent)
	    (error 'add-relation "expected symbol for parent, got: ~s" parent))
	  (unless (member child l)
	    (hash-table-put! ht parent (cons child l))))
	(unless (member child roots)
	  (set! roots (cons child roots))))))

;; build the container stress test
(define build-container-frame
  (local [(define small-message%
	    (let ([font (make-object wx:font% 9 wx:const-modern
				     wx:const-normal wx:const-normal #t)])
	      (class mred:canvas% (parent message)
		     (inherit set-min-height get-dc user-min-height user-min-width)
		     (public
		      [on-paint
		       (lambda ()
			 (let ([dc (get-dc)]
			       [h (box 0)]
			       [w (box 0)])
			   (send dc get-size w h)
			   (send dc draw-text message 0 0)))])
		     (sequence
		       (super-init parent -1 -1 -1 -1 0))
		     (private
		      [text-height
		       (let ([dc (get-dc)])
			 (send dc set-font font)
			 (send dc get-char-height))]
		      [width (* (send (get-dc) get-char-width) (string-length message))]
		      [height 20])
		     (sequence
		       (user-min-width (+ 10 width))
		       (user-min-height height)
		       (set-min-height height)))))
	  (define add-tree
	    (lambda (panel)
	      (lambda (symbol)
		(let* ([main (make-object mred:horizontal-panel% panel)]
		       [msg (make-object small-message% main (symbol->string symbol))]
		       [children-panel (make-object mred:vertical-panel% ma9n)])
		  (map (add-tree children-panel)
		       (hash-table-get ht symbol (lambda () null)))))))]
	 (lambda ()
	   (let* ([frame (make-object mred:frame% null "M3 Hierarchy")]
		  [panel (make-object mred:horizontal-panel% frame)])
	     ;(for-each (add-tree panel) roots) ;; uncomment for entire tree
	     ((add-tree panel) 'zodiac)         ;; only draw zodiac hierarchy
	     (send frame show #t)
	     frame))))


;; draws the current heirarchy into a dc 
(define draw-into-dc 
  (lambda (dc margin)
    (let ([pen (send wx:the-pen-list find-or-create-pen "BLACK" 1 wx:const-solid)]
	  [font (make-object wx:font% 9 wx:const-roman
			     wx:const-normal wx:const-normal #f)])
      (send dc set-font font)
      (send dc set-pen pen)
      (local [(define font-height (send dc get-char-height))
	      (define-struct node (label children height))
	      (define compare-node
		(lambda (x y) 
		  (let ([x-height (node-height x)]
			[y-height (node-height y)])
		    (if (= x-height y-height)
			(string-ci<=? (node-label x) (node-label y))
			(< y-height x-height)))))
	      (define build-tree
		(lambda (root)
		  (let* ([root-string (symbol->string root)]
			 [children
			  (quicksort (map build-tree (hash-table-get ht root (lambda () null)))
				     compare-node)])
		    (make-node root-string
			       children
			       (max font-height (foldl + 0 (map node-height children)))))))
	      (define draw-tree
		(lambda (node x y)
		  (let* ([height (node-height node)]
			 [label (node-label node)]
			 [children (node-children node)]
			 [text-w (let ([b (box 0)])
				   (send dc get-text-extent label b (box 0))
				   (unbox b))]
			 [line-offset 5]
			 [line-factor 1]
			 [line-width (+ (* line-factor (sqrt height)) line-offset)]
			 [line-indent 2]
			 [line-start-y (+ y (/ height 2))]
			 [line-start-x (+ x text-w line-indent)]
			 [line-end-x (+ line-start-x line-width (- (* 2 line-indent)))]
			 [string-y (- line-start-y (/ font-height 2))])
		    (send dc draw-text label x string-y)
		    (foldl (lambda (child new-y) 
			     (let ([child-height (node-height child)])
			       (draw-tree child (+ x text-w line-width) new-y)
			       (send dc draw-line
				     line-start-x line-start-y
				     line-end-x (+ new-y (/ child-height 2)))
			       (+ child-height new-y)))
			   y
			   children))))]
	(foldl (lambda (root y) (let ([tree (build-tree root)])
				  (draw-tree tree margin y)
				  (+ y (node-height tree))))
	       margin
	       (quicksort roots (lambda (x y)
				  (string-ci<=? (symbol->string x)
						(symbol->string y)))))))))

;; test the build heirarchy in a frame
(define build-canvas-frame
  (lambda ()
    (let* ([margin 20]
	   [f (make-object mred:frame% null "M3 Hierarchy" -1 -1 300 300)]
	   [p (make-object mred:horizontal-panel% f)]
	   [c (make-object 
	       (class mred:canvas% x
		(inherit get-dc)
		 (public
		   [on-paint
		    (lambda ()
		      (draw-into-dc (get-dc) margin))])
		 (sequence (apply super-init x)))
	       p)])
      (send f show #t)
      (send c stretchable-in-x #t)
      (send c stretchable-in-y #t)
      f)))

;; write the hierarchy into a postscript file
(define output-postscript
  (lambda (filename scale)
    (let ([dc (make-object wx:post-script-dc% filename)]
	  [MARGIN 36])
      (send* dc 
	(start-doc "Hierarchy")
	(start-page)
	(set-user-scale scale scale))
      (draw-into-dc dc MARGIN)
      (send* dc
	(end-page)
	(end-doc)))))
