; to be exported:
; panel%
; horizontal-panel%
; vertical-panel%
; single-panel%

; to be imported:
; make-item%
; child-info

(define mred:container-panels@
  (unit/sig mred:container-panels^
    (import [mred:debug : mred:debug^]
	    [mzlib:function : mzlib:function^]
	    mred:container-children^)
    
    (mred:debug:printf 'invoke "mred:container-panels@")
    
    (define debug-borders
      (let ([curr-value #f])
	(case-lambda
	 [() curr-value]
	 [(new-value)
	  (if (boolean? new-value)
	      (set! curr-value new-value)
	      (error 'mred:debug-borders "Expected a boolean; got ~s"
		     new-value))])))
    
    ; panel%: a class with all the functionality of a wx:panel%
    ; that can hold items and reposition them as necessary.  Note that a
    ; panel can contain other panels.
    (define panel%
      (class (make-item% wx:panel% #t #t list) args
	(inherit
	  object-ID
	  get-x
	  get-y
	  get-width
	  get-height
	  user-min-width
	  user-min-height
	  get-client-size
	  get-parent)
	
	(rename
	  [super-on-size on-size]
	  [super-set-size set-size])
	
	(private
	  
	  ; cache to prevent on-size from recomputing its result every
	  ; time. when curr-width is #f, cache invalid.
	  curr-width
	  curr-height
	  
	  ; list of child-info structs corresponding to the children.  (#f
	  ;  if no longer valid.)
	  [children-info null])
	
	(public
	  
	  [on-default-action
	   (lambda (item)
	     (send item on-default-action))]

	  ; list-diff: computes the difference between two lists
	  ; input: l1, l2: two lists
	  ; returns:  a list of all elements in l1 which are not in l2.
	  ; note: all comparisons made with eq?; algorithm suggested by
	  ; robby (note that it's O(N), not O(N^2)).
 	  [list-diff
	   (lambda (l1 l2)
	     (let ([table (make-hash-table)])
	       (for-each
		(lambda (item)
		  (hash-table-put! table item #t))
		l2)
	       (let loop ([l l1])
		 (cond
		   [(null? l) null]
		   [(hash-table-get table (car l) (lambda () #f))
		    (loop (cdr l))]
		   [else (cons (car l) (loop (cdr l)))]))))]
	  
	  ; list of panel's contents.
	  [children null]
	  
	  ; add-child: adds an existing child to the panel.
	  ; input: new-child: item% descendant to add
	  ; returns: nothing
	  ; effects: adds new-child to end of list of children.
	  [add-child
	   (lambda (new-child)
	     (unless (memq new-child children)
	       (unless (eq? this (send new-child get-parent))
		 (error 'add-child
			(string-append
			 "Attempted to add child ~s to panel ~s "
			 "(not child's parent)")
			new-child this))
	       (change-children
		(lambda (l)
		  (append l (list new-child))))
	       (when (eq? wx:window-system 'motif)
		 (send this set-item-cursor 0 0))))]
	  
	  ; change-children: changes the list of children.
	  ; input: f is a function which takes the current list of children
	  ;   and returns a new list of children.
	  ; returns: nothing
	  ; effects: sets the list of children to the value of applying f.
	  [change-children
	   (lambda (f)
	     (let ([new-children (f children)])
	       (mred:debug:printf
		'container-panel-change-children
		(string-append
		 "container-panel-change-children: "
		 "Inside a panel's change-children"))
	       (mred:debug:printf
		'container-panel-change-children
		(string-append
		 "container-panel-change-children: "
		 "Old children: ~s~nNew children: ~s")
		children new-children)
	       (unless (andmap (lambda (child)
				 (eq? this (send child get-parent)))
			       new-children)
		 (error 'change-children
			(string-append 
			 "Not all members of the new list are "
			 "children of this panel ~s~nlist: ~s")
			this new-children))
	       ; show all new children, hide all deleted children.
	       (mred:debug:printf 
		'container-panel-change-children
		(string-append
		 "container-panel-change-children: "
		 "showing and hiding children as appropriate"))
	       (let ([added-children (list-diff new-children children)]
		     [removed-children (list-diff children new-children)])
		 (for-each (lambda (child) (send child show #t))
			   added-children)
		 (for-each (lambda (child) (send child show #f))
			   removed-children))
	       (mred:debug:printf
		'container-panel-change-children
		(string-append
		 "container-panel-change-children: "
		 "Changing children list and forcing redraw."))
	       (set! children new-children)
	       (force-redraw)))]
	  
	  ; delete-child: removes a child from the panel.
	  ; input: child: child to delete.
	  ; returns: nothing
	  ; effects: removes child from list; forces redraw.
	  [delete-child
	   (lambda (child)
	     (change-children (lambda (child-list)
				(mzlib:function:remq child child-list))))]
	  
	  ; get-children-info: returns children info list, recomputing it
	  ;   if needed.
	  ; input: none
	  ; returns: list of child-info structs.
	  ; effects: upon exit, children-info is eq? to result.
	  [get-children-info
	   (lambda ()
	     (mred:debug:printf 
	      'container-panel-get-children-info
	      (string-append
	       "container-panel-get-children-info: "
	       "Entering get-children-info; object ~s")
	      object-ID)
	     (unless children-info
	       (mred:debug:printf
		'container-panel-get-children-info
		(string-append
		 "container-panel-get-children-info: "
		 "Recomputing children info"))
	       (set! children-info
		     (map (lambda (child)
			    (send child get-info))
			  children)))
	     children-info)]
	  
	  ; force-redraw: forces a redraw of the entire window.
	  ; input: none
	  ; returns: nothing
	  ; effects: sends a message up to the top container to redraw
	  ;   itself and all of its children.
	  [force-redraw
	   (lambda ()
	     (mred:debug:printf
	      'container-panel-force-redraw
	      (string-append
	       "container-panel-force-redraw: "
	       "Entering force-redraw; object ~s")
	      object-ID)
	     (set! children-info #f)
	     (set! curr-width #f)
	     (let ([parent (get-parent)])
	       (unless (null? parent)
		 (mred:debug:printf 
		  'container-panel-force-redraw
		  (string-append
		   "container-panel-force-redraw: "
		   "calling parent's force-redraw"))
		 (send parent force-redraw))))]
	  
	  ; get-min-graphical-size: poll children and return minimum possible
	  ;   size, as required by the graphical representation of the tree,
	  ;   of the panel.
	  ; input: none
	  ; returns: minimum full size (as a list, width & height) of the
	  ;   container.
	  ; effects: none
	  [get-graphical-min-size
	   (letrec ([gms-helper
		     (lambda (kid-info x-accum y-accum)
		       (if (null? kid-info)
			   (list x-accum y-accum)
			   (let ([curr-info (car kid-info)])
			     (gms-helper
			      (cdr kid-info)
			      (max x-accum
				   (+ (* 2 const-default-border)
				      (child-info-x-posn curr-info)
				      (child-info-x-min curr-info)))
			      (max y-accum
				   (+ (* 2 const-default-border)
				      (child-info-y-posn curr-info)
				      (child-info-y-min
				       curr-info)))))))])
	     (lambda ()
	       (let-values ([(client-w client-h)
			     (get-two-int-values get-client-size)])
		 (let ([min-client-size
			(gms-helper (get-children-info)
				    (* 2 const-default-border)
				    (* 2 const-default-border))]
		       [delta-w (- (get-width) client-w)]
		       [delta-h (- (get-height) client-h)])
		   (list (+ delta-w (car min-client-size))
			 (+ delta-h (cadr min-client-size)))))))]
	  
	  ; get-min-size: poll children and return minimum possible size
	  ;   for the container which considers the user min sizes.
	  ; input: none
	  ; returns: minimum full size (as a list, width & height) of
	  ;   container.
	  ; effects: none.
	  [get-min-size
	   (lambda ()
	     (let ([graphical-min-size (get-graphical-min-size)])
	       (list
		(max (car graphical-min-size) (user-min-width))
		(max (cadr graphical-min-size) (user-min-height)))))]
	  
	  ; set-size:
	  [set-size
	   (lambda (x y width height)
	     (mred:debug:printf 
	      'container-panel-set-size
	      (string-append
	       "container-panel-set-size: entering; "
	       "args ~s ~s ~s ~s; object ID ~s")
	      x y width height object-ID)
	     (if (and (same-dimension? x (get-x))
		      (same-dimension? y (get-y))
		      (same-dimension? width (get-width))
		      (same-dimension? height (get-height)))
		 (begin
		   (mred:debug:printf
		    'container-panel-set-size
		    "container-panel-set-size: redrawing children")
		   (call-with-values
		    (lambda ()
		      (get-two-int-values get-client-size))
		    redraw))
		 (begin
		   (mred:debug:printf
		    'container-panel-set-size
		    "container-panel-set-size: calling super-set-size")
		   (super-set-size x y width height))))]
	  
	  ; on-size: called when the container is resized (usu by its
          ;   parent) 
	  ; input: new-width/new-height: new size of panel
	  ; returns: nothing
	  ; effects: causes children to redraw themselves.
	  [on-size
	   (lambda (new-width new-height)
	     (mred:debug:printf
	      'container-panel-on-size
	      "container-panel-on-size: Entering; args: ~s x ~s; object ~s"
	      new-width new-height object-ID)
	     (mred:debug:printf
	      'container-panel-on-size
	      "container-panel-on-size: current size: ~s ~s"
	      (get-width) (get-height))
	     (mred:debug:printf
	      'container-panel-on-size
	      "container-panel-on-size: calling overridden method")
	     (super-on-size new-width new-height)
	     (mred:debug:printf
	      'container-panel-on-size
	      "container-panel-on-size: Current size: ~s ~s"
	      (get-width) (get-height))
	     (let-values ([(client-width client-height)
			   (get-two-int-values get-client-size)])
	       (if (and (number? curr-width)
			(number? curr-height)
			(= curr-width client-width)
			(= curr-height client-height))
		   (mred:debug:printf
		    'container-panel-on-size
		    (string-append
		     "Container-panel-on-size: "
		     "same size so not redrawing."))
		   (begin
		     (mred:debug:printf
		      'container-panel-on-size
		      "container-panel-on-size: Client size: ~s x ~s"
		      client-width client-height)
		     (redraw client-width client-height)))))]
	  
	  ; place-children: determines where each child of panel should be
	  ; placed.
	  ; input: children-info: list of mred:child-info structs
	  ;          corresponding to children.
	  ;        width/height: size of panel's client area.
	  ; returns: list of placement info for children; each item in list
	  ;   is a list of 4 elements, consisting of child's x-posn,
	  ;   y-posn, x-size, y-size.  Items are in same order as
	  ;   children-info list.
	  [place-children
	   (lambda (children-info width height)
	     (if (null? children-info)
		 null
		 (let ([curr-info (car children-info)])
		   (cons
		    (list
		     (child-info-x-posn curr-info)
		     (child-info-y-posn curr-info)
		     (child-info-x-min curr-info)
		     (child-info-y-min curr-info))
		    (place-children (cdr children-info) width height)))))]
	  
	  ; redraw: redraws panel and all children
	  ; input: width, height: size of drawable area in panel.
	  ; returns: nothing
	  ; effects: places children at default positions in panel.
	  [redraw
	   (letrec ([redraw-helper
		     (lambda (children placement-info)
		       (unless (null? children)
			 (let ([curr-child (car children)])
			   (apply
			    (ivar curr-child set-size)
			    (car placement-info))
			   (redraw-helper
			    (cdr children)
			    (cdr placement-info)))))])
	     (lambda (width height)
	       (mred:debug:printf
		'container-panel-redraw
		"container-panel-redraw: Redrawing panel's children")
	       (redraw-helper
		children
		(place-children (get-children-info) width height))))])
	(sequence
	  (apply super-init
		 (apply 
		  (opt-lambda (parent
			       [x const-default-posn]
			       [y const-default-posn]
			       [w const-default-size]
			       [h const-default-size]
			       [style 0] . args)
		    (list* parent x y w h
			   (if (debug-borders)
			       (bitwise-ior style wx:const-border)
			       style)
			   args))
		  args)))))
    
    ; make-get-graphical-size: creates a function which returns the minimum
    ;   possible size for a horizontal-panel% or vertical-panel% object.
    ; input: container: a pointer to the panel% descendant upon
    ;          which this function operates.
    ;        compute-x/compute-y: functions which take the current x/y
    ;          location, the amount of spacing which will come after the
    ;          current object, and the list of child-info structs beginning
    ;          with the current object, and return the new x/y locations.
    ;        spacing: (defaults to const-default-spacing).  The size of
    ;          the gap between adjacent objects and objects and the edge
    ;          of the panel.
    ; returns: a thunk which returns the minimum possible size of the
    ;   entire panel (not just client) as a list of two elements:
    ;   (min-x min-y). 
    (define make-get-graphical-size
      (lambda (container compute-x compute-y)
	(letrec ([gms-help
		  (lambda (kid-info x-accum y-accum)
		    (if (null? kid-info)
			(list x-accum y-accum)
			(gms-help
			 (cdr kid-info)
			 (compute-x x-accum kid-info)
			 (compute-y y-accum kid-info))))])
	  (lambda ()
	    (let-values ([(client-w client-h)
			  (get-two-int-values
			   (ivar container get-client-size))])
	      (let* ([border (send container border)]
		     [min-client-size
		      (gms-help (send container get-children-info)
				(* 2 border) (* 2 border))]
		     [delta-w (- (send container get-width) client-w)]
		     [delta-h (- (send container get-height) client-h)])
		(list (+ delta-w (car min-client-size))
		      (+ delta-h (cadr min-client-size)))))))))
    
    ; make-h-v-redraw: creates place-children functions for
    ; horizontal-panel% or vertical-panel% classes.
    ; input: container: pointer to the panel% object which uses
    ;          the resulting function.
    ;        child-major-size: function which takes a child-info struct
    ;          and returns the child's minimum size in the major direction
    ;          of the panel.
    ;        child-major-stretch: function which takes a mred;child-info
    ;          struct and returns the child's stretchability in the major
    ;          direction of the panel.
    ;        child-minor-size/child-minor-stretch: see above.
    ;        major-dim/minor-dim: functions which take the width and the
    ;          height of the panel and return the panel's major and minor
    ;          dimensions, respectively.
    ;        get-h-info/get-v-info: functions which take info lists
    ;          describing the major and minor directions and select the
    ;          appropriate one.
    ; returns: a function which takes the children info, the width and the
    ;   height of the panel's client and returns a list which contains
    ;   posn&size info for each child. 
    (define make-place-children
      (lambda (container child-major-size
			 child-major-stretch
			 child-minor-size
			 child-minor-stretch
			 major-dim minor-dim
			 get-x-info get-y-info)
	(lambda (kid-info width height)
	  (letrec ([count-stretchable
		    (lambda (kid-info)
		      (if (null? kid-info)
			  0
			  (let ([curr-info (car kid-info)])
			    (if (child-major-stretch curr-info)
				(add1 (count-stretchable (cdr kid-info)))
				(count-stretchable (cdr kid-info))))))])
	    (let* ([spacing (send container spacing)]
		   [border (send container border)]
		   [full-w (send container get-width)]
		   [full-h (send container get-height)]
		   [delta-list (list
				(- full-w width)
				(- full-h height))]
		   [num-stretchable (count-stretchable kid-info)]
		   [extra-space (- (major-dim width height)
				   (- (apply 
				       major-dim
				       (send container get-graphical-min-size))
				      (apply major-dim delta-list)))]
		   [extra-per-stretchable (if (zero? num-stretchable)
					      0
					      (inexact->exact
					       (round
						(/ extra-space
						   num-stretchable))))]
		   [num-children (length kid-info)])
	      (letrec
		  ([pc-help
		    (lambda (kid-info left-edge)
		      (if (null? kid-info)
			  null
			  (let* ([curr-info (car kid-info)]
				 [major-posn left-edge]
				 [major-size
				  (if (child-major-stretch curr-info)
				      (+ extra-per-stretchable
					 (child-major-size curr-info))
				      (child-major-size curr-info))]
				 [minor-posn (if (child-minor-stretch
						  curr-info)
						 border
						 (inexact->exact
						  (round
						   (/ (- (minor-dim
							  width
							  height)
							 (child-minor-size
							  curr-info))
						      2))))]
				 [minor-size (if (child-minor-stretch
						  curr-info)
						 (- (minor-dim width height)
						    (* 2 border))
						 (child-minor-size
						  curr-info))])
			    (cons
			     (list
			      (get-x-info major-posn minor-posn)
			      (get-y-info major-posn minor-posn)
			      (get-x-info major-size minor-size)
			      (get-y-info major-size minor-size))
			     (pc-help (cdr kid-info)
				      (+ major-size major-posn spacing))))))])
		(mred:debug:printf
		 'container-panel-redraw
		 "container-panel-redraw: redrawing panel's children")
		(pc-help kid-info border)))))))
    
    (define make-spacing
      (lambda (panel)
	(let ([curr-spacing (ivar panel default-spacing-width)])
	  (case-lambda
	   [() curr-spacing]
	   [(new-val)
	    (unless (non-negative-number? new-val)
	      (error 'spacing
		     "Expected a non-negative number; given ~s"
		     new-val))
	    (set! curr-spacing new-val)
	    (send panel force-redraw)]))))
    
    (define make-border
      (lambda (panel)
	(let ([curr-border (ivar panel default-border-width)])
	  (case-lambda
	   [() curr-border]
	   [(new-val)
	    (unless (non-negative-number? new-val)
	      (error 'border
		     "Expected a non-negative number; given ~s"
		     new-val))
	    (set! curr-border new-val)
	    (send panel force-redraw)]))))
    
    ; horizontal-panel%: a panel which arranges its children in an evenly
    ; spaced horizontal row.  Items are vertically centered (or stretched
    ; to fit the dialog box if they are stretchable).  The items are evenly
    ; spaced horizontally, with any extra space divided evenly among the
    ; stretchable items. 
    (define horizontal-panel%
      (class-asi panel%
	
	(public
	  [default-spacing-width const-default-spacing]
	  [default-border-width const-default-border]
	  
	  [spacing (make-spacing this)]
	  
	  [border (make-border this)]
	  
	  [get-graphical-min-size
	   (make-get-graphical-size 
	    this
	    (lambda (x-accum kid-info)
	      (+ x-accum (child-info-x-min (car kid-info))
		 (if (null? (cdr kid-info))
		     0
		     (spacing))))
	    (lambda (y-accum kid-info)
	      (max y-accum
		   (+ (child-info-y-min (car kid-info))
		      (* 2 (border))))))]
	  
	  [place-children
	   (make-place-children
	    this
	    child-info-x-min
	    child-info-x-stretch
	    child-info-y-min
	    child-info-y-stretch
	    (lambda (width height) width)
	    (lambda (width height) height)
	    (lambda (major minor) major)
	    (lambda (major minor) minor))])))
    
    ; vertical-panel%.  See horizontal-panel%, but reverse
    ; "horizontal" and "vertical."
    (define vertical-panel%
      (class-asi panel%
	
	(public
	  [default-spacing-width const-default-spacing]
	  [default-border-width const-default-border]
	  
	  [spacing (make-spacing this)]
	  
	  [border (make-border this)]
	  
	  [get-graphical-min-size
	   (make-get-graphical-size
	    this
	    (lambda (x-accum kid-info)
	      (max x-accum
		   (+ (child-info-x-min (car kid-info))
		      (* 2 (border)))))
	    (lambda (y-accum kid-info)
	      (+ y-accum (child-info-y-min (car kid-info))
		 (if (null? (cdr kid-info))
		     0
		     (spacing)))))]
	  
	  [place-children
	   (make-place-children this
				child-info-y-min
				child-info-y-stretch
				child-info-x-min
				child-info-x-stretch
				(lambda (width height) height)
				(lambda (width height) width)
				(lambda (major minor) minor)
				(lambda (major minor) major))])))
    
    (define add-at-end
      (lambda (object)
	(lambda (list-of-kids)
	  (append list-of-kids (list object)))))
    
    ; implement a panel which can hold multiple objects but only displays
    ; one at a time.  The size of the panel is the smallest size possible
    ; for displaying each of the panel's children.
    (define single-panel%
      (class panel% args
	
	(inherit
	  object-ID
	  children
	  force-redraw)
	
	(rename
	  [super-add add-child]
	  [super-delete delete-child])
	
	(public
	  
	  ; pointer to currently active child
	  [active null]
	  
	  [default-border-width const-default-border]
	  
	  [border (make-border this)]
	  
	  [add-child
	   (lambda (new-child)
	     (super-add new-child)
	     (send new-child show #f))]
	  
	  ; if the child is active, make the next child active (null if
	  ; child was last in list)
	  [delete-child
	   (lambda (child)
	     (when (eq? child (active-child))
	       (let ([rest-of-list (cdr (memq child children))])
		 (active-child (if (null? rest-of-list)
				   null
				   (car rest-of-list)))))
	     (super-delete child))]
	  
	  ; if the active child is removed, make nothing active.
	  [change-children
	   (lambda (f)
	     (let ([new-children (f children)])
	       (mred:debug:printf
		'container-panel-change-children
		"container-panel-change-children: Changing children")
	       (mred:debug:printf 
		'container-panel-change-children
		(string-append
		 "container-panel-change-children: "
		 "old children ~s~nnewchildren ~s")
		children new-children)
	       (unless (andmap (lambda (child)
				 (eq? this (send child get-parent)))
			       new-children)
		 (error 'change-children
			(string-append
			 "not all children in the new list "
			 "have this panel ~s as their parent~nnew list: ~s")
			this new-children))
	       (unless (memq (active-child) new-children)
		 (active-child null))
	       (mred:debug:printf 
		'container-panel-change-children
		(string-append
		 "container-panel-change-children: "
		 "setting children and forcing redraw"))
	       (set! children new-children)
	       (force-redraw)))]
	  
	  [active-child
	   (case-lambda
	    [() active]
	    [(new-child)
	     (unless (or (null? new-child) 
			 (eq? this (send new-child get-parent)))
	       (error 'active-child
		      (string-append
		       "The child specified (~s) is not "
		       "a child of this panel (~s)")
		      new-child this))
	     (unless (null? active) (send active show #f))
	     (unless (null? new-child) (send new-child show #t))
	     (set! active new-child)
	     (force-redraw)])]
	  
	  [get-graphical-min-size
	   (make-get-graphical-size
	    this
	    (lambda (x-accum kid-info)
	      (max x-accum (+ (* 2 (border))
			      (child-info-x-min (car kid-info)))))
	    (lambda (y-accum kid-info)
	      (max y-accum (+ (* 2 (border))
			      (child-info-y-min (car kid-info))))))]
	  
	  ; only place the active child.
	  [place-children
	   (lambda (children-info width height)
	     (mred:debug:printf
	      'container-panel-place-children
	      "Entering place-children; object ~s" object-ID)
	     (unless (null? active)
	       (mred:debug:printf 'container-panel-place-children
				  "Placing active child")
	       (let* ([active-info (send active get-info)]
		      [x-stretch (child-info-x-stretch active-info)]
		      [x-min (child-info-x-min active-info)]
		      [y-stretch (child-info-y-stretch active-info)]
		      [y-min (child-info-y-min active-info)]
		      [x-posn (if x-stretch
				  (border)
				  (/ (- width x-min) 2))]
		      [x-size (if x-stretch
				  (- width (* 2 (border)))
				  x-min)]
		      [y-posn (if y-stretch
				  (border)
				  (/ (- height y-min) 2))]
		      [y-size (if y-stretch
				  (- height (* 2 (border)))
				  y-min)])
		 (list x-posn y-posn x-size y-size))))]
	  
	  [redraw
	   (lambda (width height)
	     (mred:debug:printf
	      'container-panel-redraw
	      "container-panel-redraw: Entering redraw; object ~s"
	      object-ID)
	     (unless (null? active)
	       (apply
		(ivar active set-size)
		; we don't really care about the children info...
		(place-children null width height))))])
	(sequence
	  (mred:debug:printf 'container-single-panel
			     "About to call single-panel's super-init")
	  (mred:debug:printf 'container-single-panel
			     "Function: ~s  Args: ~s" super-init args)
	  (apply super-init args))))))
