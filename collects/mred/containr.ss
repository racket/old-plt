; resizes too small really confuse things, since client sizes wrap (-2
; becomes 65534.  Matthew knows; I'm waiting to hear from him before I do
; anything.)

;-----------------------------------------------------------------------

; exported ID's:
;  const-default-size
;  const-default-posn
;  const-default-spacing
;  make-child-info, etc.
;  frame%
;  dialog-box%
;  canvas%
;  media-canvas%
;  button%
;  check-box%
;  choice%
;  gauge%
;  list-box%
;  message%
;  radio-box%
;  slider%
;  text-window%
;  text%
;  multi-text%
;  panel%
;  horizontal-panel%
;  vertical-panel%
;  single-panel%

(define mred:container@
  (unit/sig mred:container^
    (import [mred:debug : mred:debug^]
            [mzlib:function : mzlib:function^])
    
    (mred:debug:printf 'invoke "mred:container@")
    
    ; this constant is used by several MrEd primitives to signify the default
    ; size of an object.
    (define const-default-size -1)
    (define const-default-posn -1);; ditto for position.
    
    ; default spacing between items.
    (define const-default-spacing 10)
    
    (define counter 0)
    
    ; this structure holds the information that a child will need to send
    ; to its parent when the parent must resize itself.
    ;  x-posn/y-posn: numbers which indicate the default position of the
    ;    child. 
    ;  x-min/y-min: numbers which indicate the minimum size of the child.
    ;  x-stretch/y-stretch: booleans which indicate whether the child can
    ;    stretch in the appropriate direction to fill otherwise empty
    ;    space. 
    (define-struct child-info (x-posn y-posn x-min y-min
				      x-stretch y-stretch))
    
    
    ; last-to-first: moves the last element in a list to the first;
    ; leaves the other elmts in the same order.
    ; input: l is a list.
    ; returns: a copy of l with the last top-level element moved to the
    ;   first position in the list; the other elements are unchanged and they
    ;   are in the same position relative to each other.
    (define last-to-first
      (lambda (l)
	(letrec ([get-last
		   (lambda (l)
		     (cond
		       [(null? (cdr l)) (car l)]
		       [else (get-last (cdr l))]))]
		 [get-rest
		   (lambda (l)
		     (cond
		       [(null? (cdr l)) null]
		       [else (cons (car l) (get-rest (cdr l)))]))])
	  (if (null? l)
	      null
	      (cons (get-last l) (get-rest l))))))

    ; get-two-int-results: a wrapper around functions that need to return
    ;   two results.
    ; input: function: a function which takes two boxes and returns results
    ;          in them.
    ; returns: the contents of the two boxes (as multiple values)
    (define get-two-int-results
      (lambda (function)
	(let ([a (box 0)]
	      [b (box 0)])
	  (function a b)
	  (values (unbox a) (unbox b)))))
    
    ; make-item%: creates items which are suitable for placing into
    ;  containers.
    ; input: item%: a wx:item% descendant (but see below) from which the
    ;          new class will be derived.
    ;        stretch-x?/stretch-y?: booleans which specify the default
    ;          stretchability behavior for the new class.
    ;        make-default-size: a function which takes the args of the
    ;          item%  class and returns a list of these args but ensures
    ;          that the size args are both -1 (default size).
    ; returns: a class, descended from wx:item%, which is suitable for
    ;            placing in a container.
    ; Note: the item% parameter does not necessarily HAVE to be a
    ; descendant of wx:item%, so long as it contains the identifiers in the
    ; inherit section below.  You will note below that I ran wx:panel%
    ; through this function to create panel%.  (Yes, this is
    ; cheating.  So what's your point?)
    (define make-item%
      (lambda (item% stretch-x? stretch-y? make-default-size)
	(class item% args
	  (inherit
	    get-width
	    get-height
	    get-x
	    get-y
	    get-parent
	    set-size)
	  
	  (private
	    
	    ; the "default position" of the object; defined to be the
            ; creation position of the object.
	    default-x
	    default-y
	    
	    ; sets the default position fields of the object based on the
            ; object's current position.  Designed to be called from the
	    ; object's constructor.
	    [set-default-posn
	      (lambda ()
		(set! default-x (get-x))
		(set! default-y (get-y)))]
	    

	    ; input: init-value: initial value for the param (moved to #t
	    ;          if not boolean).
	    ;        function-name: a symbol, intended to be the name of
	    ;          the created function (used in error).
	    ; returns: a function which behaves exactly like the result of
	    ;   make-parameter (out of mzlib); if the argument is not
	    ;   boolean, it is interpreted as #t; after the parameter is
	    ;   set, force-redraw is called.
	    [make-stretchable-parameter
	      (lambda (init-value function-name)
		(let ([parm-value init-value])
		  (case-lambda
		    [() parm-value]
		    [(new-value) (if (boolean? new-value)
				     (begin
				       (set! parm-value new-value)
				       (force-redraw))
				     (error function-name
					    "Expected a boolean; given ~s"
					    new-value))])))])
	  
	  (public
	    
	    ; a unique numeric ID for the object (for debugging).
	    object-ID
	    
	    ; store minimum size of item.  Since we no longer support label
	    ; changes, this will never change, so we don't have to maintain
            ; a validity indicator.
	    min-width
	    min-height
	    
	    ; stretchable-in-x?: gets/sets horiz stretchability property.
	    ; input: either nothing or a boolean.
	    ; returns: (if nothing passed in) current stretchability
            ;   property. 
	    ; effects: (if boolean value passed in) sets H stretchability
	    ;   prop. to specified value.
	    [stretchable-in-x?
	      (make-stretchable-parameter stretch-x? 'stretchable-in-x?)]
	    
	    ; stretchable-in-y?: see stretchable-in-x? but substitute "y"
            ;   for "x" and "horizontal" for "vertical".
	    [stretchable-in-y?
	      (make-stretchable-parameter stretch-y? 'stretchable-in-y?)]
	    
	    ; get-info: passes necessary info up to parent.
	    ; input: none
	    ; returns: child-info struct containing the info about this
	    ;   item.
	    ; intended to be called by item's parent upon resize.
	    [get-info
	      (lambda ()
		(mred:debug:printf 'container
		  "Entering get-info; object ~s" object-ID)
		(let* ([min-size (get-min-size)]
		       [result (make-child-info default-x default-y
						(car min-size) (cadr min-size)
						(stretchable-in-x?)
						(stretchable-in-y?))])
		  (mred:debug:printf 'container "Result: ~s" result)
		  result))]
	    
	    ; force-redraw: unconditionally trigger redraw.
	    ; input: none
	    ; returns: nothing
	    ; effects: forces the item's parent (if it exists) to redraw
	    ;   itself. This will recompute the min-size cache if it is
	    ;   invalid.
	    [force-redraw
	      (lambda ()
		(mred:debug:printf 'container
		  "Entering force-redraw; object ~s" object-ID)
		(let ([parent (get-parent)])
		  (unless (null? parent)
		    (send parent force-redraw))))]
	    
	    ; get-min-size: computes the minimum size the item can
            ;   reasonably assume.
	    ; input: none
	    ; returns: a list containing the minimum width & height.
	    [get-min-size
	      (lambda ()
		(mred:debug:printf 'container "get-min-size; object ~s;  "
		  object-ID)
		(mred:debug:printf 'container "Result:  ~s"
		  (list min-width min-height))
		(list min-width min-height))])
	  (sequence
	    (apply super-init (apply make-default-size args))
	    (let ([x (get-width)]
		  [y (get-height)])
	      (set! min-width x)
	      (set! min-height y)

	      (set! object-ID counter)
	      (set! counter (add1 counter))
	      (set-default-posn)
	      (let ([parent (car args)])  ; this, at least, is consistent
		(cond
		  [(or (is-a? parent frame%)
		       (is-a? parent dialog-box%))
		   (send parent insert-panel this)]
		  [(is-a? parent panel%)
		   (send parent add-child this)]
		  [else (error 'init
			  "Expected a mred:frame%, mred:dialog-box%, ~s ~s"
			  "or mred:panel% for parent.  Received"
			  parent)])))))))
    
    ; Justification for make-default-size, and (class item% args ..):
    ; The various classes that we pass through this function do not all
    ; have their parameter lists in the same order, so the position and
    ; size args tend to move around.  There are two alternatives: either
    ; change the parameter lists so that they are more consistent, or the
    ; method used here.  The first solution would make modifying existing
    ; code to use the item% classes significantly more difficult, as the
    ; parameter lists would also have to be reordered.
    
    ; standard-make-default-size: takes a parameter list and modifies it so
    ; that the item constructed must have the default size.
    ; input: parent, callback, label, x, y, w, h, other args.  See docs for
    ;   wx:button% for details.
    ; returns: a list of the parameters with const-default-size
    ;   substituted in for w and h.
    ; Note: this function works with wx:button%, wx:check-box%, wx:choice%,
    ; and wx:radio-box%.
    (define standard-make-default-size
      (opt-lambda (parent callback label
			  [x const-default-posn]
			  [y const-default-posn]
			  [w const-default-size]
			  [h const-default-size] . args)
	(append (list parent callback label x y const-default-size
		      const-default-size)
	  args)))
    
    ; these next definitions descend classes from the children of wx:item%
    ; which can be inserted into panel% objects.
    (define button%
      (make-item% wx:button% #t #t standard-make-default-size))
    
    (define check-box%
      (make-item% wx:check-box% #f #f standard-make-default-size))
    
    (define choice%
      (make-item% wx:choice% #t #f standard-make-default-size))
    
    (define gauge%
      (class (make-item% wx:gauge% #t #f list) args
	(inherit
	  get-client-size
	  get-width
	  get-height
	  set-size
	  stretchable-in-x?
	  stretchable-in-y?
	  min-height
	  min-width)
	(private
	  ; # pixels per unit of value.
	  [pixels-per-value 1]
	  
	  ; sets minimum sizes based on constructor args.
	  ; input: range: range as passed to constructor
	  ;        style: style as passed to constructor
	  ; returns: nothing
	  ; effects: sets major dimension of slider to pixels-per-value *
          ; range, plus enough for borders and label text (if appropriate).
          ; Sets minor dimension of slider to default minor dimension, plus
	  ; room for label if needed.
	  [set-min-sizes
	    (lambda (range style)
	      (let-values ([(client-width client-height)
			    (get-two-int-results get-client-size)])
		(let ([delta-w (- (get-width) client-width)]
		      [delta-h (- (get-height) client-height)]
		      [horizontal? (positive? (bitwise-and style
						wx:const-horizontal))])
		  (mred:debug:printf 'min-size
		    "entering gauge set-min-size; args ~s ~s"
		    range style)
		  (mred:debug:printf 'min-size
		    "client size: ~s x ~s"
		    client-width client-height)
		  (mred:debug:printf 'min-size
		    "actual size: ~s x ~s"
		    (get-width) (get-height))
		  (set! min-width (if horizontal?
				      (+ (* range pixels-per-value)
					 delta-w)
				      ; client-height is the default
				      ; dimension in the minor direction.
				      (+ client-height delta-w)))
		  (set! min-height (if horizontal?
				       (+ client-height delta-h)
				       (+ (* range pixels-per-value)
					  delta-h))))))])
	(sequence
	  (let ([new-args
		  (apply (opt-lambda
			   (parent label range
			     [x const-default-posn]
			     [y const-default-posn]
			     [w const-default-size]
			     [h const-default-size]
			     [style wx:const-horizontal] . args)
			   (append (list parent label range x y
				     const-default-size const-default-size
				     style)
			     args))
		    args)])
	    (mred:debug:printf 'min-size "Args to gauge: ~s" new-args)
	    (apply super-init new-args)
	    (apply (lambda (parent lable range x y w h style . args)
		     (set-min-sizes range style)
		     (if (positive? (bitwise-and style
				      wx:const-horizontal))
			 ; see slider% for discussion of why force-redraw
			 ; is unnecessary here.
			 (begin
			   (stretchable-in-x? #t)
			   (stretchable-in-y? #f))
			 (begin
			   (stretchable-in-x? #f)
			   (stretchable-in-y? #t))))
	      new-args)))))
			 
	
    
    (define list-box%
      (make-item% wx:list-box% #t #t
		  (opt-lambda (parent callback label
				      [multiple-selection wx:const-single]
				      [x const-default-posn]
				      [y const-default-posn]
				      [w const-default-size]
				      [h const-default-size] . args)
		    (append (list parent callback label multiple-selection x y
				  const-default-size const-default-size)
			    args))))
    
    (define message%
      (make-item% wx:message% #t #t list))
    ; we don't need to process the size args at all cause there aren't
    ; any. Therefore, we just need to bundle the args up in a list.  list
    ; already does that.
    
    (define radio-box%
      (make-item% wx:radio-box% #f #f standard-make-default-size))
    
    (define slider%
      (class (make-item% wx:slider% #f #f list) args
	(inherit
	  min-width
	  min-height
	  stretchable-in-x?
	  stretchable-in-y?
	  get-client-size
	  get-width
	  get-height)
	(private
	  ; # pixels per possible setting.
	  [pixels-per-value 3]

	  ; default value for the width parameter (which we ignore
	  ; completely).
	  [default-width 150]
	  
	  ; sets minimum sizes based on constructor args
	  ; input: min-val/max-val: range of values slider can assume
	  ;        style: style of slider as passed to constructor
	  ; returns: nothing
	  ; effects: sets the major dimension of the slider to be
	  ;            pixels-per-value * range size, plus a bit for borders
	  ;            & label (calculated accurately at runtime)
	  ;          sets the minor dimension of the slider to be the
	  ;            default minor dimension of the slider.
	  [set-min-size
	    (lambda (min-val max-val style)
	      (let-values ([(client-w client-h)
			    (get-two-int-results get-client-size)])
		(let ([full-width (get-width)]
		      [full-height (get-height)]
		      [range (add1 (- max-val min-val))]
		      [horizontal? (positive? (bitwise-and style
						wx:const-horizontal))])
		  (mred:debug:printf 'min-size
		    "Entering slider's set-min-size; args ~s ~s ~s"
		    min-val max-val style)
		  (mred:debug:printf 'min-size
		    "Client size: ~s x ~s"
		    client-w client-h)
		  (mred:debug:printf 'min-size
		    "Full size: ~s x ~s"
		    full-width full-height)
		  (set! min-width
		    (if horizontal?
			(+ (* range pixels-per-value)
			   (- full-width client-w))
			full-width))
		  (set! min-height
		    (if horizontal?
			full-height
			(+ (* range pixels-per-value)
			   (- full-height client-h)))))))])
	
	(sequence
	  (apply super-init
	    (apply
	      (opt-lambda    ; force width to default value
		(parent func label value min max width
		  [x const-default-posn]
		  [y const-default-posn]
		  [style wx:const-horizontal] . args)
		(append (list parent func label value min max default-width
			  x y style)
		  args))
	      args))
	  (apply (opt-lambda     ; set min size, stretchability.
		   (parent func label value min max width
		     [x const-default-posn]
		     [y const-default-posn]
		     [style wx:const-horizontal] . args)
		   (set-min-size min max style)
		   ; the stretchability adjustments reset the parent's
		   ; child-info cache for us, so (unlike canvas) we don't
		   ; have to force a redraw.
		   (if (positive? (bitwise-and style wx:const-horizontal))
		       (begin
			 (stretchable-in-x? #t)
			 (stretchable-in-y? #f))
		       (begin
			 (stretchable-in-x? #f)
			 (stretchable-in-y? #t))))
	    args))))
    
    (define text%;; for now
      (make-item% wx:text% #t #f
		  (opt-lambda (parent callback label
				      [val ""]
				      [x const-default-posn]
				      [y const-default-posn]
				      [w const-default-size]
				      [h const-default-size] . args)
		    (append (list parent callback label val x y
				  const-default-size const-default-size)
			    args))))
    
    (define multi-text%
      (make-item% wx:multi-text% #t #t
		  (opt-lambda (parent callback label
				      [val ""]
				      [x const-default-posn]
				      [y const-default-posn]
				      [w const-default-size]
				      [h const-default-size] . args)
		    (append (list parent callback label val x y
				  const-default-size const-default-size)
			    args))))
    
    (define make-canvas%
      (lambda (base%)
	(class (make-item% base% #t #t
			   (opt-lambda (parent [x const-default-posn]
					       [y const-default-posn]
					       [w const-default-size]
					       [h const-default-size] . args)
			     (append (list parent x y const-default-size
					   const-default-size)
				     args)))
	  args
	  
	  (inherit
	    force-redraw
	    get-client-size
	    get-height
	    get-width
	    min-height
	    min-width)
	  
	  (private
	    ; the smallest possible client size for the canvas.
	    [smallest-client-size 5]
	    
	    ; find-min-size: determines the minimum possible size for the
	    ; canvas.
	    ; input: none
	    ; returns: nothing
	    ; effects: sets min-height and min-width to the smallest
	    ;   possible size of the canvas (defined to be that size which
	    ;   results in the client area of the canvas being 5 x 5).
	    [find-min-size
	      (lambda ()
		(let-values ([(width height)
			      (get-two-int-results get-client-size)])
		  (mred:debug:printf 'media-canvas
		    "Entering find-min-size.  client size: ~s x ~s"
		    width height)
		  (mred:debug:printf 'media-canvas
		    "Full size: ~s x ~s" (get-width) (get-height))
		  (let* ([delta-x (- (get-width) width)]
			 [delta-y (- (get-height) height)])
		    (set! min-height (+ smallest-client-size delta-y))
		    (set! min-width (+ smallest-client-size delta-x))
		    ; we have to invalidate the parent's child-info cache
		    ; because we got added to it before we computed our
		    ; minimum size.
		    (force-redraw))))])
	  (sequence
	    (apply super-init args)
	    (find-min-size)))))
    
    (define canvas% (make-canvas% wx:canvas%))
    (define media-canvas% (make-canvas% wx:media-canvas%))		    
    (define text-window% (make-canvas% wx:text-window%))
    
    ; panel%: a class with all the functionality of a wx:panel%
    ; that can hold items and reposition them as necessary.  Note that a
    ; panel can contain other panels.
    (define panel%
      (class-asi (make-item% wx:panel% #t #t list)
	(inherit
	  object-ID
	  get-width
	  get-height
	  get-client-size
	  get-parent)
	
	(private
	  
	  ; cache to prevent on-size from recomputing its result every
	  ; time. when curr-width is #f, cache invalid.
	  curr-width
	  curr-height
	  
	  ; list of child-info structs corresponding to the children.  (#f
	  ;  if no longer valid.)
	  [children-info null])
	  
	(public
	  
	  ; list of panel's contents.
	  [children null]
	  
	  ; add-child: adds an existing child to the panel.
	  ; input: new-child: item% descendant to add
	  ;        show?: #t to show child immediately, else #f.
	  ; returns: nothing
	  ; effects: adds new-child to end of list of children.
	  [add-child
	   (opt-lambda (new-child [show? #t])
	     (unless (memq new-child children)
	       (unless (eq? this (send new-child get-parent))
		 (error 'add-child
		   "Attempted to add child ~s to panel ~s (not child's parent)"
		   new-child this))
	       (send new-child show show?)
	       (change-children (add-at-end new-child))))]
	  
	  ; change-children: changes the list of children.
	  ; input: f is a function which takes the current list of children
	  ;   and returns a new list of children.
	  ; returns: nothing
	  ; effects: sets the list of children to the value of applying f.
	  [change-children
	   (lambda (f)
	     (set! children (f children))
	     (force-redraw))]
	  
	  ; delete-child: removes a child from the panel.
	  ; input: child: child to delete.
	  ; returns: nothing
	  ; effects: removes child from list; forces redraw.
	  [delete-child
	   (lambda (child)
	     (send child show #f)
	     (change-children (lambda (child-list)
				(mzlib:function:remq child child-list))))]
	  
	  ; get-children-info: returns children info list, recomputing it
	  ;   if needed.
	  ; input: none
	  ; returns: list of child-info structs.
	  ; effects: upon exit, children-info is eq? to result.
	  [get-children-info
	   (lambda ()
	     (mred:debug:printf 'container
				"Entering get-children-info; object ~s"
				object-ID)
	     (unless children-info
	       (mred:debug:printf 'container "Recomputing children info")
	       (set! children-info (map (lambda (child)
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
	     (mred:debug:printf 'container
				"Entering force-redraw; object ~s" object-ID)
	     (set! children-info #f)
	     (set! curr-width #f)
	     (let ([parent (get-parent)])
	       (if (null? parent)
		   (let-values ([(width height)
				 (get-two-int-results get-client-size)])
		     (redraw width height))
		   (send parent force-redraw))))]
	  
	  ; get-min-size: poll children and return minimum possible size
	  ;   for the container.
	  ; input: none
	  ; returns: minimum size (as a list, width & height) of
	  ; container.
	  ; effects: none.
	  [get-min-size
	   (letrec ([gms-helper
		     (lambda (kid-info x-accum y-accum)
		       (if (null? kid-info)
			   (list x-accum y-accum)
			   (let ([curr-info (car kid-info)])
			     (gms-helper
			      (cdr kid-info)
			      (max x-accum
				   (+ const-default-spacing
				      (child-info-x-posn curr-info)
				      (child-info-x-min curr-info)))
			      (max y-accum
				   (+ const-default-spacing
				      (child-info-y-posn curr-info)
				      (child-info-y-min
				       curr-info)))))))])
	     (lambda ()
	       (let-values ([(client-w client-h)
			     (get-two-int-results get-client-size)])
		 (let ([min-client-size
			 (gms-helper (get-children-info)
			   const-default-spacing const-default-spacing)]
		       [delta-w (- (get-width) client-w)]
		       [delta-h (- (get-height) client-h)])
		   (list (+ delta-w (car min-client-size))
		         (+ delta-h (cadr min-client-size)))))))]
		     
	  
	  ; on-size: called when the container is resized (usu by its
          ;   parent) 
	  ; input: new-width/new-height: new size of panel
	  ; returns: nothing
	  ; effects: causes children to redraw themselves.
	  [on-size
	    (lambda (new-width new-height)
	      (mred:debug:printf 'container
				 "Entering on-size; object ID ~s;  " object-ID)
	      (mred:debug:printf 'container
				 "New size: ~s x ~s" new-width new-height)
	      (let-values ([(client-width client-height)
			    (get-two-int-results get-client-size)])
		(mred:debug:printf 'container
				   "Client size: ~s x ~s" client-width
				   client-height)
		(unless (and (number? curr-width)
			     (number? curr-height)
			     (= client-width curr-width)
			     (= client-height curr-height))
		  (set! curr-width client-width)
		  (set! curr-height client-height)
		  (mred:debug:printf 'container
				     "On-size is forcing a redraw.")
		  (redraw client-width client-height))))]
	  

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
		      (place-children (cdr children-info))))))]

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
	       (redraw-helper children
		 (place-children (get-children-info) width height))))])))
    
    (define make-top-container%
      (lambda (base%)
	(class base% args
	  (inherit
	    set-size
	    get-width
	    get-height
	    get-client-size)
	  
	  (private
	    
	    ; pointer to panel in the frame for use in on-size
	    [panel null])
	  (public
	    
	    ; a unique ID for the object for debugging purposes.
	    object-ID
	      
	    ; insert-panel: update panel pointer.
	    ; input: new-panel: panel in frame (descendant of
	    ;   mred:panel%) 
	    ; returns: nothing
	    ; effects: sets panel to new-panel
	    ;          if new-panel is not a descendant of
	    ;            mred:panel%, calls error; panel not updated.
	    [insert-panel
	      (lambda (new-panel)
		(mred:debug:printf 'container
		  "Entering insert-panel, object ~s" object-ID)
		(mred:debug:printf 'container
		  "Argument: ~s; ID ~s" new-panel
		  (ivar new-panel object-ID))
		(unless (is-a? new-panel panel%)
		  (error 'insert-panel
		    "Expected a mred:panel% descendant; got ~s"
		    new-panel))
		(unless (eq? this (send panel get-parent))
		  (error 'insert-panel
		    "Added panel ~s to a frame (~s) not its parent"
		    new-panel this))
		(set! panel new-panel)
		(send panel set-size 0 0 (get-width) (get-height))
		(force-redraw))]
	    
	    [get-panel
	      (lambda ()
		panel)]

	    [force-redraw
	      (lambda ()
		(mred:debug:printf 'container
		  "Entering force-redraw; object ~s" object-ID)
		(on-size (get-width) (get-height)))]
	    
	    ; on-size: ensures that size of frame matches size of content
	    ; input: new-width/new-height: new size of frame
	    ; returns: nothing
	    ; effects: if new size is smaller than allowed size of
	    ;            contents, frame resized to smallest possible size.
	    ;            If frame is larger than contents and contents
	    ;            aren't stretchable, frame resized to size of
	    ;            contents.  Each direction is handled
	    ;            independantly.
	    [on-size
	      (lambda (new-width new-height)
		(mred:debug:printf 'container
		  "FRAME: Entered frame's on-size; args ~s ~s"
		  new-width new-height)
		(unless (null? panel)
		  (let ([p-x (box 0)]
			[p-y (box 0)])
		    (send panel get-client-size p-x p-y)
		    (let* ([panel-info (send panel get-info)]
			   
			   ; minimum size assumable by client area of panel
			   [min-width (child-info-x-min panel-info)]
			   [min-height (child-info-y-min panel-info)]
			   
			   ; panel's client size
			   [p-client-width (unbox p-x)]
			   [p-client-height (unbox p-y)]
			   
			   ; difference between panel's client & full
			   ; sizes. 
			   [p-delta-w (- (send panel get-width)
					 p-client-width)]
			   [p-delta-h (- (send panel get-height)
					 p-client-height)]
			   
			   ; difference between frame's full size & panel's
			   ; client size.
			   [delta-w (- new-width p-client-width)]
			   [delta-h (- new-height p-client-height)]
			   
			   ; new size for panel's client area
			   [new-w (cond
				    [(< p-client-width min-width)
				     min-width]
				    [(and (> p-client-width min-width)
					  (not (child-info-x-stretch
						panel-info)))
				     min-width]
				    [else p-client-width])]
			   [new-h (cond
				    [(< p-client-height min-height)
				     min-height] 
				    [(and (> p-client-height min-height)
					  (not (child-info-y-stretch
						panel-info)))
				     min-height]
				    [else p-client-height])]
			   
			   ; new frame size, computed from above
			   ; quantities.
			   [f-width (+ new-w delta-w)]
			   [f-height (+ new-h delta-h)])
		      (mred:debug:printf 'container
					 "FRAME: panel client ~s x ~s"
					 p-client-width p-client-height)
		      (mred:debug:printf 'container
					 "FRAME: size differences: ~s, ~s"
					 delta-w delta-h)
		      (mred:debug:printf 'container
					 "FRAME: New size: ~s x ~s"
					 new-w new-h)
		      (send panel set-size const-default-posn
			    const-default-posn
			    (+ (- f-width delta-w) p-delta-w)
			    (+ (- f-height delta-h) p-delta-h))
		      (unless (and (= new-width f-width)
				   (= new-height f-height))
			(mred:debug:printf 'container
					   "FRAME: Resizing to ~s x ~s"
					   f-width f-height)
			(set-size const-default-posn const-default-posn
				  f-width f-height)))))
		(mred:debug:printf 'container
				   "FRAME: Leaving onsize at the end."))])
	  (sequence
	    (apply super-init args)
	    (set! object-ID counter)
	    (set! counter (add1 counter))))))
    
    (define frame% (make-top-container% wx:frame%))
    (define dialog-box% (make-top-container% wx:dialog-box%))
    
    ; make-get-size: creates a function which returns the minimum possible
    ;   size for a horizontal-panel% or vertical-panel% object.
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
    ;   entire panel as a list of two elements: (min-x min-y).
    (define make-get-size
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
			  (get-two-int-results
			    (ivar container get-client-size))])
	      (let* ([border (send container border)]
		     [min-client-size
		       (gms-help (send container get-children-info)
			 border border)]
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
    ;        spacing: the size of the gaps between adjacent objects and
    ;          objects and the edge of the panel.
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
				   (- (apply major-dim
					(send container get-min-size))
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
		(pc-help kid-info border)))))))

    (define make-spacing
      (lambda (panel)
	(let ([curr-spacing const-default-spacing])
	  (case-lambda
	    [() curr-spacing]
	    [(new-val)
	     (unless (and (real? new-val)
		       (not (negative? new-val)))
	       (error 'spacing
		 "Expected a non-negative number; given ~s"
		 new-val))
	     (set! curr-spacing new-val)
	     (send panel force-redraw)]))))

    (define make-border
      (lambda (panel)
	(let ([curr-border const-default-spacing])
	  (case-lambda
	    [() curr-border]
	    [(new-val)
	     (unless (and (real? new-val)
		       (not (negative? new-val)))
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
	(inherit
	  get-children-info
	  force-redraw)
	
	(public
	  [spacing (make-spacing this)]

	  [border (make-border this)]
	  
	  [get-min-size
	    (make-get-size this
	      (lambda (x-accum kid-info)
		(+ x-accum (child-info-x-min (car kid-info))
		  (if (null? (cdr kid-info))
		      (border)
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
	(inherit
	  force-redraw)
	(public
	  [spacing (make-spacing this)]

	  [border (make-border this)]
	  
	  [get-min-size
	    (make-get-size this
	      (lambda (x-accum kid-info)
		(max x-accum
		  (+ (child-info-x-min (car kid-info))
		     (* 2 (border)))))
	      (lambda (y-accum kid-info)
		(+ y-accum (child-info-y-min (car kid-info))
		  (if (null? (cdr kid-info))
		      (border)
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
	  
	  [border (make-border this)]

	  [add-child
	   (lambda (new-child)
	     (super-add new-child #f))]

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
		  
	  [active-child
	   (case-lambda
	    [() active]
	    [(new-child)
	     (unless (null? active) (send active show #f))
	     (unless (null? new-child) (send new-child show #t))
	     (set! active new-child)
	     (force-redraw)])]

	  [get-min-size
	    (make-get-size
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
	      (mred:debug:printf 'container
		"Entering place-children; object ~s" object-ID)
	      (unless (null? active)
		(mred:debug:printf 'container
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
	      (mred:debug:printf 'container
		"Entering redraw; object ~s" object-ID)
	      (unless (null? active)
		(apply
		  (ivar active set-size)
		  ; we don't really care about the children info...
		  (place-children null width height))))])
	(sequence
	  (mred:debug:printf 'container
	    "About to call single-panel's super-init")
	  (mred:debug:printf 'container
	    "Function: ~s  Args: ~s" super-init args)
	  (apply super-init args))))))
