#|

A bundle-manager% is an manager class. It keeps track of an instance
of a Bundle Contents and the views associated with that instance.
It supports three methods:

  get-bundle : (-> bundle<%>)
    returns the bundle that this manager manages

  create-view : (-> void)
    constructs a snip that displays the Bundle that this manager
    manages. Calls its argument with the snip, which must insert
    the snip into an editor%.
  
  bundle-changed : (-> void)
    notifies all views of this bundle to update themselves.
    
-- 

A Bundle is a tree representing the structure of the bundle,
using the interpreter pattern:

            bundle%
              |
       +--------------+
       |              |
 node-bundle%   leaf-bundle%
  

 bundle% supports:
   

    set-bundle-manager : (bundle-manager<%> -> void)
      sets the anchor (used to notify views of modifications)

 <<<<<<< bundle.ss
    traverse : ((bundle A -> A) A -> A)
      visits the entire tree (leaves and nodes) by calling the function argument
      at each node. The second argument is used as the initial second argument to
      the function argument, after that the result of one call to the function is used
      as the input for the next call to the function.

    get-flat-names : (-> (list-of symbols))
      
  bundle-leaf% supports:
  
     get-names : (-> (list-of symbol))
     set-names : ((list-of symbol) -> void)

  bundle-node% supports:
    
    get-label     : (-> symbol)
    set-label     : (symbol -> void)
    set-children  : ((list-of bundle%) -> void)
    get-children  : (-> (list-of bundle%))
    add-child     : (bundle% -> void)

--

bundle-pasteboard%

leaf-bundle-snip%
node-bundle-snip%
    
|#

 =======
    traverse : ((bundle A -> A) A -> A)
      visits the entire tree (leaves and nodes) by calling the function argument
      at each node. The second argument is used as the initial second argument to
      the function argument, after that the result of one call to the function is used
      as the input for the next call to the function.

  bundle-leaf% supports:
  
     get-names : (-> (list-of symbol))
     set-names : ((list-of symbol) -> void)

  bundle-node% supports:
    
    get-label     : (-> symbol)
    set-label     : (symbol -> void)
    set-children  : ((list-of bundle%) -> void)
    get-children  : (-> (list-of bundle%))
    add-child     : (bundle% -> void)
     
--

bundle-pasteboard%

leaf-bundle-snip%
node-bundle-snip%
    
 |#

 >>>>>>> 1.9
(invoke-unit/sig
 (unit/sig ()
   (import mred^)
   
   (define bundle-manager<%>
     (interface ()
       get-bundle
       create-view
       bundle-changed))
   
   (define bundle-manager%
     (class* object% (bundle-manager<%>) (contents)
       (private
	 [views null]
	 [interior-height-addition 10]
	 [calculate-tree-size
	  (lambda (view)
	    (let o-loop ([contents-snip (send view get-contents-snip)])
	      (let ([contents (send contents-snip get-bundle)])
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
		   (let i-loop ([bundle
				 (send contents-snip get-bundle-snips)]
				[width 0]
				[height 0])
		     (cond
		       [(null? bundle)
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
		       [else (let*-values ([(c-width c-height) (o-loop (car bundle))])
			       (i-loop (cdr bundle)
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
		 (let* ([bundle (send contents-snip get-bundle)]
			[tree-width (send bundle get-tree-width)]
			[width (send contents-snip get-width)])
		   (send view move-to contents-snip (+ x (/ (- tree-width width) 2)) y))
		 
		 ;; loop over children
		 (let ([text-space (let ([yt (box 0)]
					 [yb (box 0)])
				     (send view get-snip-location contents-snip #f yt #f)
				     (send view get-snip-location contents-snip #f yb #t)
				     (- (unbox yb) (unbox yt)))])
		   (let i-loop ([bundle-snips (send contents-snip get-bundle-snips)]
				[x x])
		     (cond
		       [(null? bundle-snips) (void)]
		       [else (let* ([bundle-content-snip (car bundle-snips)]
				    [bundle-content (send bundle-content-snip get-bundle)]
				    
				    [tree-width (send bundle-content get-tree-width)]
				    [tree-height (send bundle-content get-tree-width)])
			       (o-loop bundle-content-snip 
				       x 
				       (+ y interior-height-addition text-space))
			       (i-loop (cdr bundle-snips)
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
					  (map loop (send contents get-bundle)))])
		   (send view insert snip)
		   snip)]
		[else (error 'create-view "fell off cond: ~e~n" contents)])))])
       (public
	 [get-bundle (lambda () contents)]
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
	 [bundle-changed
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
	       (lambda (c x) (send c set-bundle-manager this))
	       (void)))))
   
   (define bundle<%>
     (interface ()
       set-bundle-manager
       get-flat-names
       traverse ; ((bundle A -> A) A -> A)
       ))
   
   (define bundle%
     (class* object% (bundle<%>) ()
       
       (private
	 [tree-width 0]
	 [tree-height 0])
       (public
	 [get-tree-width (lambda () tree-width)]
	 [get-tree-height (lambda () tree-height)]
	 [set-tree-width (lambda (w) (set! tree-width w))]
	 [set-tree-height (lambda (h) (set! tree-height h))])
       
       (private
	 [bundle-manager #f])
       (public
	 [get-bundle-manager
	  (lambda ()
	    bundle-manager)]
	 [set-bundle-manager
	  (lambda (b)
	    (unless (is-a? b bundle-manager<%>)
	      (error 'set-bundle-manager "expected a bundle-manager<%>, got: ~e"
		     b))
	    (set! bundle-manager b))]
	 [traverse
	  (lambda (f init)
	    (void))])
       (sequence
	 (super-init))))
   
   (define leaf-bundle<%>
     (interface (bundle<%>)
       get-names
       set-names))
   
   (define leaf-bundle%
     (class* bundle% (leaf-bundle%) (_names)
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
   
   (define node-bundle<%>
     (interface (bundle<%>)
       get-label
       set-label
       set-children
       get-children
       add-child))
   
   (define node-bundle%
     (class bundle% (_label _children)
       (override
	[traverse
	 (lambda (f init)
	   (let loop ([contents bundle]
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
	 [bundle _bundle])
       (public
	 [get-label
	  (lambda ()
	    label)]
	 [set-label
	  (lambda (s)
	    (unless (symbol? s)
	      (error 'set-names "expected a list of symbols, got: ~e"
		     s))
	    (set! label s))]
         [get-children
	  (lambda ()
	    children)]
	 [set-children
	  (lambda (chils)
	    (unless (and (list? chils)
			 (andmap (lambda (x) (is-a? x bundle<%>)) chils))
	      (error 'set-names "expected a list of bundle<%> objects, got: ~e" bc))
	    (set! children chils))])

       (inherit get-bundle-manager)
       (public
	 [add-child
	  (lambda (c)
	    (set! bundle (cons c bundle))
	    (send (get-bundle-manager) bundle-changed))])
       (sequence
	 (super-init)
	 (set-label _label)
	 (set-children _children))))
   
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
       (inherit invalidate-bitmap-cache)
       (override
	[after-move-to
	 (lambda (snip x y dragging)
	   (invalidate-bitmap-cache))]
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
		      (let i-loop ([bundle-snips
				    (send contents-snip get-bundle-snips)])
			(cond
			  [(null? bundle-snips) (void)]
			  [else
			   (let* ([bundle-content-snip (car bundle-snips)])
			     (let ([bx (get-snip-x-location bundle-content-snip)]
				   [by (get-snip-top-location bundle-content-snip)])
			       (send dc draw-line (+ x dx) (+ y dy) (+ bx dx) (+ by dy))
			       (o-loop bundle-content-snip)
			       (i-loop (cdr bundle-snips))))])))]
		   [else (error 'on-paint "fell off cond: ~e~n" contents-snip)]))
	       (send dc set-pen pen))))])
       (sequence
	 (super-init))))
   
   (define leaf-bundle-snip%
     (class editor-snip% (leaf-bundle)
       (public
	 [get-bundle
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
   
   (define node-bundle-snip%
     (class snip% (node-bundle bundle-snips)
       (public
	 [get-bundle
	  (lambda ()
	    node-bundle)]
	 [get-bundle-snips
	  (lambda ()
	    bundle-snips)])
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
   (define bundle (make-object bundle-manager% int2))
   
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
	     (let ([node-bundle (send snip get-bundle)])
	       (send node-bundle add-child (make-object leaf-bundle% '(zzz)))))))))

   (define (new-node) (void))
   
   (define (new-bundle)
     (let ([name (get-text-from-user "New bundle" "Name of new bundle")])
       (send text insert name)
       (send bundle create-view (lambda (snip) (send text insert snip)))
       (send text insert #\newline)))
   
   (send frame show #t))
 mred^)