(invoke-unit/sig
 (unit/sig ()
   (import mred^)
   
   (define bundle<%>
     (interface ()
       create-view))
   
   (define bundle%
     (class* object% (bundle<%>) (contents)
       (private
	 [views null]
	 [interior-height-addition 10]
	 [calculate-tree-size
	  (lambda (view)
	    (let o-loop ([contents-snip (send view get-contents-snip)])
	      (let ([contents (send contents-snip get-bundle-contents)])
		(cond
		  [(is-a? contents-snip leaf-bundle-snip%)
		   (let ([xl (box 0)]
			 [xr (box 0)]
			 [yt (box 0)]
			 [yb (box 0)])
		     (send view get-snip-location contents-snip xl yt #f)
		     (send view get-snip-location contents-snip xr yb #t)
		     (let ([w (- (unbox xr) (unbox xl))]
			   [h (- (unbox yb) (unbox yt))])
		       (send contents set-tree-width w)
		       (send contents set-tree-height h)
		       (values w h)))]
		  [(is-a? contents-snip node-bundle-snip%)
		   (let i-loop ([bundle-contents
				 (send contents-snip get-bundle-contents-snips)]
				[width 0]
				[height 0])
		     (cond
		       [(null? bundle-contents)
			(let ([xl (box 0)]
			      [yt (box 0)]
			      [xr (box 0)]
			      [yb (box 0)])
			  (send view get-snip-location contents-snip xl yt #f)
			  (send view get-snip-location contents-snip xr yb #t)
			  (let ([w (max width (- (unbox xr) (unbox xl)))]
				[h (+ height interior-height-addition
				      (- (unbox yb) (unbox yt)))])
			    (send contents set-tree-width w)
			    (send contents set-tree-height h)
			    (values w h)))]
		       [else (let*-values ([(c-width c-height) (o-loop (car bundle-contents))])
			       (i-loop (cdr bundle-contents)
				       (+ c-width width)
				       (max c-height height)))]))]
		  [else (error 'position-view-contents
			       "fell off cond: ~e~n"
			       contents-snip)]))))]
	 [position-snips
	  (lambda (view)
	    (let o-loop ([contents-snip (send view get-contents-snip)]
			 [x 0]
			 [y 0])
	      (cond
		[(is-a? contents-snip leaf-bundle-snip%)
		 (send view move-to contents-snip x y)]
		[(is-a? contents-snip node-bundle-snip%)
		 
		 ;; set this snips position
		 (let* ([bundle-contents (send contents-snip get-bundle-contents)]
			[tree-width (send bundle-contents get-tree-width)]
			[width (send contents-snip get-width)])
		   (send view move-to contents-snip (+ x (/ (- tree-width width) 2)) y))
		 
		 ;; loop over children
		 (let ([text-space (let ([yt (box 0)]
					 [yb (box 0)])
				     (send view get-snip-location contents-snip #f yt #f)
				     (send view get-snip-location contents-snip #f yb #t)
				     (- (unbox yb) (unbox yt)))])
		   (let i-loop ([bundle-contents-snips (send contents-snip get-bundle-contents-snips)]
				[x x])
		     (cond
		       [(null? bundle-contents-snips) (void)]
		       [else (let* ([bundle-content-snip (car bundle-contents-snips)]
				    [bundle-content (send bundle-content-snip get-bundle-contents)]
				    
				    [tree-width (send bundle-content get-tree-width)]
				    [tree-height (send bundle-content get-tree-width)])
			       (o-loop bundle-content-snip 
				       x 
				       (+ y interior-height-addition text-space))
			       (i-loop (cdr bundle-contents-snips)
				       (+ x tree-width)))])))]
		[else (error 'position-snips "fell off cond: ~e~n" contents-snip)])))]
	 [build-view-contents
	  (lambda (view)
	    (let loop ([contents contents])
	      (cond
		[(is-a? contents leaf-bundle%)
		 (let ([snip (make-object leaf-bundle-snip% contents)])
		   (send view insert snip)
		   snip)]
		[(is-a? contents node-bundle%)
		 (let ([snip (make-object node-bundle-snip%
					  contents
					  (map loop (send contents get-bundle-contents)))])
		   (send view insert snip)
		   snip)]
		[else (error 'create-view "fell off cond: ~e~n" contents)])))])
       (public
	 [create-view
	  (lambda (insert-into-editor)
	    (let* ([view (make-object bundle-pasteboard%)]
		   [snip (make-object editor-snip% view)])
	      (insert-into-editor snip)
	      (send view set-contents-snip (build-view-contents view))
	      (calculate-tree-size view)
	      (position-snips view)
	      (set! views (cons view views))
	      snip))]
	 [contents-changed
	  (lambda ()
	    (unless (null? views)
	      (let ([f
		     (lambda (calc-size)
		       (lambda (view)
			 
			 (let ([old-snips
				(let loop ([snip (send view find-first-snip)])
				  (if snip
				      (cons snip (loop (send snip next)))
				      null))])
			   
			   ;; delete the old snips (why does this break things?)
			   (for-each
			     (lambda (snip) (send view delete snip))
			     old-snips)

			   ;; build new ones
			   (send view set-contents-snip (build-view-contents view))
			   (calc-size view)
			   (position-snips view))))])
		
		(for-each (f calculate-tree-size) views))))])
       (sequence
	 (super-init)
	 (send contents traverse
	       (lambda (c x) (send c set-bundle this))
	       (void)))))
   
   (define bundle-contents<%>
     (interface ()
       set-bundle
       traverse ; ((bundle-contents A -> A) A -> A)
       ))
   
   (define bundle-contents%
     (class* object% (bundle-contents<%>) ()
       
       (private
	 [tree-width 0]
	 [tree-height 0])
       (public
	 [get-tree-width (lambda () tree-width)]
	 [get-tree-height (lambda () tree-height)]
	 [set-tree-width (lambda (w) (set! tree-width w))]
	 [set-tree-height (lambda (h) (set! tree-height h))])
       
       (private
	 [bundle #f])
       (public
	 [get-bundle
	  (lambda ()
	    bundle)]
	 [set-bundle
	  (lambda (b)
	    (unless (is-a? b bundle<%>)
	      (error 'set-bundle "expected a bundle<%>, got: ~e"
		     b))
	    (set! bundle b))]
	 [traverse
	  (lambda (f init)
	    (void))])
       (sequence
	 (super-init))))
   
   (define leaf-bundle%
     (class bundle-contents% (_names)
       (private
	 [names _names])
       (override
	[traverse
	 (lambda (f init)
	   (f this init))])
       (public
	 [get-names
	  (lambda ()
	    names)]
	 [set-names
	  (lambda (n)
	    (unless (and (list? n)
			 (andmap symbol? n))
	      (error 'set-names "expected a list of symbols, got: ~e"
		     n))
	    (set! names n))])
       (sequence
	 (super-init)
	 (set-names _names))))
   
   (define node-bundle%
     (class bundle-contents% (_label _bundle-contents)
       (override
	[traverse
	 (lambda (f init)
	   (let loop ([contents bundle-contents]
		      [init init])
	     (cond
	       [(null? contents) (f this init)]
	       [else (loop
		      (cdr contents)
		      (send (car contents)
			    traverse
			    (lambda (object init) (f object init))
			    init))])))])
       (private
	 [label _label]
	 [bundle-contents _bundle-contents])
       (public
	 [get-label
	  (lambda ()
	    label)]
	 [get-bundle-contents
	  (lambda ()
	    bundle-contents)]
	 [set-bundle-contents
	  (lambda (bc)
	    (unless (and (list? bc)
			 (andmap (lambda (x) (is-a? x bundle-contents<%>)) bc))
	      (error 'set-names "expected a list of symbols, got: ~e" bc))
	    (set! bundle-contents bc))]
	 [set-label
	  (lambda (s)
	    (unless (symbol? s)
	      (error 'set-names "expected a list of symbols, got: ~e"
		     s))
	    (set! label s))])

       (inherit get-bundle)
       (public
	 [add-child
	  (lambda (c)
	    (set! bundle-contents (cons c bundle-contents))
	    (send (get-bundle) contents-changed))])
       (sequence
	 (super-init)
	 (set-label _label)
	 (set-bundle-contents _bundle-contents))))
   
   (define bundle-pasteboard%
     (class pasteboard% ()
       (private
	 [contents-snip #f])
       (public
	 [get-contents-snip (lambda () contents-snip)]
	 [set-contents-snip (lambda (c) (set! contents-snip c))])
       
       (inherit get-snip-location)
       (private
	 [get-snip-x-location
	  (lambda (snip)
	    (let ([xl (box 0)]
		  [xr (box 0)])
	      (get-snip-location snip xl #f #f)
	      (get-snip-location snip xr #f #t)
	      (floor (+ (unbox xl) (/ (- (unbox xr) (unbox xl)) 2)))))]
	 [get-snip-top-location
	  (lambda (snip)
	    (let ([yt (box 0)])
	      (get-snip-location snip #f yt #f)
	      (unbox yt)))]
	 [get-snip-bottom-location
	  (lambda (snip)
	    (let ([yb (box 0)])
	      (get-snip-location snip #f yb #t)
	      (unbox yb)))])
       (override
	[on-paint
	 (lambda (before? dc left top right bottom dx dy draw-caret)
	   (when (and contents-snip
		      (not before?))
	     (let ([pen (send dc get-pen)])
	       (set-dc-pen dc "BLUE" 1 'solid)
	       (let o-loop ([contents-snip contents-snip])
		 (cond
		   [(is-a? contents-snip leaf-bundle-snip%) (void)]
		   [(is-a? contents-snip node-bundle-snip%)
		    (let ([x (get-snip-x-location contents-snip)]
			  [y (get-snip-bottom-location contents-snip)])
		      (let i-loop ([bundle-contents-snips
				    (send contents-snip get-bundle-contents-snips)])
			(cond
			  [(null? bundle-contents-snips) (void)]
			  [else
			   (let* ([bundle-content-snip (car bundle-contents-snips)])
			     (let ([bx (get-snip-x-location bundle-content-snip)]
				   [by (get-snip-top-location bundle-content-snip)])
			       (send dc draw-line (+ x dx) (+ y dy) (+ bx dx) (+ by dy))
			       (o-loop bundle-content-snip)
			       (i-loop (cdr bundle-contents-snips))))])))]
		   [else (error 'on-paint "fell off cond: ~e~n" contents-snip)]))
	       (send dc set-pen pen))))])
       (sequence
	 (super-init))))
   
   (define leaf-bundle-snip%
     (class editor-snip% (leaf-bundle)
       (public
	 [get-bundle-contents
	  (lambda ()
	    leaf-bundle)])
       (private
	 [text (make-object text%)]
	 [update-text
	  (lambda ()
	    ;(send text begin-edit-sequence)
	    (let ([names (send leaf-bundle get-names)])
	      (unless (null? names)
		(send text insert (symbol->string (car names)))
		(for-each (lambda (name)
			    (send text insert #\newline)
			    (send text insert (symbol->string name)))
			  (cdr names))))
	    ;(send text end-edit-sequence)
	    )])
       (sequence
	 (update-text)
	 (super-init text))))
   
   (define (set-box/f! b/f contents)
     (when (box? b/f)
       (set-box! b/f contents)))
   (define (set-dc-pen dc color width style)
     (send dc set-pen (send the-pen-list find-or-create-pen color width style)))
   (define (set-dc-brush dc color style)
     (send dc set-brush (send the-brush-list find-or-create-brush color style)))
   (define white (make-object color% "WHITE"))
   (define black (make-object color% "BLACK"))
   
   (define node-bundle-snip%
     (class snip% (node-bundle bundle-contents-snips)
       (public
	 [get-bundle-contents
	  (lambda ()
	    node-bundle)]
	 [get-bundle-contents-snips
	  (lambda ()
	    bundle-contents-snips)])
       (private
	 [width 10]
	 [height 10])
       (public
	 [get-width (lambda () width)]
	 [get-height (lambda () height)])
       (override
	[get-extent
	 (lambda (dc x y w h descent space lspace rspace)
	   
	   (let-values ([(tw th _1 _2)
			 (send dc get-text-extent
			       (symbol->string (send node-bundle get-label)))])
	     (set! width tw)
	     (set! height th))
	   
	   (set-box/f! w width)
	   (set-box/f! h height)
	   (set-box/f! descent 0)
	   (set-box/f! space 0)
	   (set-box/f! lspace 0)
	   (set-box/f! rspace 0))]
	[draw
	 (lambda (dc x y left top right bottom dx dy draw-caret)
	   (let ([foreground (send dc get-text-foreground)]
		 [background (send dc get-text-background)]
		 [mode (send dc get-text-mode)])
	     (send dc set-text-foreground black)
	     (send dc set-text-background white)
	     (send dc set-text-mode 'solid)
	     (send dc draw-text (symbol->string (send node-bundle get-label)) x y)
	     (send dc set-text-mode mode)
	     (send dc set-text-foreground foreground)
	     (send dc set-text-background background)))])
       (sequence
	 (super-init))))
   
   ;; test
   (define leaf1 (make-object leaf-bundle% '(aaa b)))
   (define leaf2 (make-object leaf-bundle% '(c dd)))
   (define int1 (make-object node-bundle% 'z (list leaf1 leaf2)))
   (define int2 (make-object node-bundle% 'y (list leaf2 int1 leaf2)))
   (define int3 (make-object node-bundle% 'x (list int2 leaf2)))
   (printf "~s~n"
	   (send int3 traverse
		 (lambda (c x)
		   (cond
		     [(is-a? c leaf-bundle%)
		      (append (send c get-names) x)]
		     [(is-a? c node-bundle%)
		      (map (lambda (x) (list (send c get-label) x))
			   x)]
		     [else (error)]))
		 null))
   (define bundle (make-object bundle% int2))
   
   (define frame (make-object frame% "Bundles" #f 400 400))
   (define button-panel (make-object horizontal-panel% frame))
   (send button-panel stretchable-height #f)
   (define text (make-object text%))
   (define new-bundle-button (make-object button%
					  "New Bundle"
					  button-panel
					  (lambda x
					    (new-bundle))))
   (define new-leaf-button (make-object button%
					"New Leaf"
					button-panel
					(lambda x
					  (new-leaf))))
   (define new-node-button (make-object button%
					"New Node"
					button-panel
					(lambda x
					  (new-node))))
   (define canvas (make-object editor-canvas%
			       frame
			       text))
   
   (define (new-leaf) 
     (let/ec k
       (let ([out
	      (lambda ()
		(bell)
		(k #f))]
	     [snip (send text get-focus-snip)])
	 (printf "~a~n" snip)
	 (unless (is-a? snip editor-snip%)
	   (out))
	 (let ([pb (send snip get-editor)])
	   (printf "~a~n" pb)
	   (unless (is-a? pb bundle-pasteboard%)
	     (out))
	   (let ([snip (send pb find-next-selected-snip #f)])
	     (printf "~a~n" snip)
	     (unless (and snip
			  (is-a? snip node-bundle-snip%)
			  (not (send pb find-next-selected-snip snip)))
	       (out))
	     (let ([node-bundle (send snip get-bundle-contents)])
	       (send node-bundle add-child (make-object leaf-bundle% '(zzz)))))))))

   (define (new-node) (void))
   
   (define (new-bundle)
     (let ([name (get-text-from-user "New bundle" "Name of new bundle")])
       (send text insert name)
       (send bundle create-view (lambda (snip) (send text insert snip)))
       (send text insert #\newline)))
   
   (send frame show #t))
 mred^)