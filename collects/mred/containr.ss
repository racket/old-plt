; resizes too small really confuse things, since client sizes wrap (-2
; becomes 65534.  Matthew knows; I'm waiting to hear from him before I do
; anything.)

; see if you can cache the frame's sizes, since the frame's onsize method
; gets called repeatedly with the same sizes.

; default size for gauges & sliders...determine upon first drawing.
; measure size of text, add to size of body (calculated from params
; easily).

;-----------------------------------------------------------------------

; define list of exported ID's
(define-signature mred:container^
  (const-default-size
    const-default-posn
    const-default-spacing
    frame%
    dialog-box%
    button%
    check-box%
    choice%
    ;gauge%
    list-box%
    message%
    radio-box%
    slider%
    text-window%
    text%
    multi-text%
    panel%
    horizontal-panel%
    vertical-panel%
    single-panel%))

(define mred:container@
  (unit/s mred:container^
    (import [mred:debug mred:debug^]
	    [mzlib:function mzlib:function^])
    ; this constant is used by several MrEd primitives to signify the default
    ; size of an object.
    (define const-default-size -1)
    (define const-default-posn -1)  ;; ditto for position.

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
	    
	    ; the "default position" of the object; defined to be the
            ; creation position of the object.
	    default-x
	    default-y

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
		(printf "Entering get-info; object ~s~n" object-ID)
		(let* ([min-size (get-min-size)]
		       [result (make-child-info default-x default-y
				 (car min-size) (cadr min-size)
				 (stretchable-in-x?) (stretchable-in-y?))])
		  (printf "Result: ~s~n" result)
		  result))]
	    
	    [set-default-posn
	      (lambda ()
		(set! default-x (get-x))
		(set! default-y (get-y)))]
	    
	    ; force-redraw: unconditionally trigger redraw.
	    ; input: none
	    ; returns: nothing
	    ; effects: forces the item's parent (if it exists) to redraw
	    ;   itself. This will recompute the min-size cache if it is
	    ;   invalid.
	    [force-redraw
	      (lambda ()
		(printf "Entering force-redraw; object ~s~n" object-ID)
		(let ([parent (get-parent)])
		  (unless (null? parent)
		    (send parent force-redraw))))]
	    
	    ; get-min-size: computes the minimum size the item can
            ;   reasonably assume.
	    ; input: none
	    ; returns: a list containing the minimum width & height.
	    [get-min-size
	      (lambda ()
		(printf "get-min-size; object ~s;  " object-ID)
		(printf "Result:  ~s~n" (list min-width min-height))
		(list min-width min-height))])
	  (sequence
	    (apply super-init (apply make-default-size args))
	    (let ([x (get-width)]
		  [y (get-height)])
	      (set! min-width x)
	      (set! min-height y)
	      (set! object-ID counter)
	      (set! counter (add1 counter))
	      (set-default-posn))))))

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
	`(,parent ,callback ,label ,x ,y ,const-default-size
	   ,const-default-size ,@args)))
    
    ; these next definitions descend classes from the children of wx:item%
    ; which can be inserted into panel% objects.
    (define button%
      (make-item% wx:button% #t #t standard-make-default-size))
    
    (define check-box%
      (make-item% wx:check-box% #f #f standard-make-default-size))
    
    (define choice%
      (make-item% wx:choice% #t #f standard-make-default-size))
    
    ;(define gauge%
    ;  (make-item% wx:gauge% #t #f))
    
    (define list-box%
      (make-item% wx:list-box% #t #t
	(opt-lambda (parent callback label
		      [multiple-selection wx:const-single]
		      [x const-default-posn]
		      [y const-default-posn]
		      [w const-default-size]
		      [h const-default-size] . args)
	  `(,parent ,callback ,label ,multiple-selection ,x ,y
	     ,const-default-size ,const-default-size
	     ,@args))))
    
    (define message%
      (make-item% wx:message% #t #t list))
    ; we don't need to process the size args at all cause there aren't
    ; any. Therefore, we just need to bundle the args up in a list.  list
    ; already does that.
    
    (define radio-box%
      (make-item% wx:radio-box% #f #f standard-make-default-size))
    
    (define slider%
      (make-item% wx:slider% #t #f list))

    (define text-window%
      (make-item% wx:text-window% #t #t
	(opt-lambda (parent
		      [x const-default-posn]
		      [y const-default-posn]
		      [w const-default-size]
		      [h const-default-size] . args)
	  `(,parent ,x ,y ,const-default-size ,const-default-size
	     ,@args))))
    
    (define text%  ;; for now
      (make-item% wx:text% #t #f
	(opt-lambda (parent callback label
		      [val ""]
		      [x const-default-posn]
		      [y const-default-posn]
		      [w const-default-size]
		      [h const-default-size] . args)
	  `(,parent ,callback ,label ,val ,x ,y
	     ,const-default-size ,const-default-size
	     ,@args))))
    
    (define multi-text%
      (make-item% wx:multi-text% #t #t
	(opt-lambda (parent callback label
		      [val ""]
		      [x const-default-posn]
		      [y const-default-posn]
		      [w const-default-size]
		      [h const-default-size] . args)
	  `(,parent ,callback ,label ,val ,x ,y
	     ,const-default-size ,const-default-size
	     ,@args))))
    
    ; define an intermediate panel, which we immediately override.  This
    ; shouldn't ever be used.
    (define intermediate-panel%
      (make-item% wx:panel% #t #t list))
    
    ; panel%: a class with all the functionality of a wx:panel%
    ; that can hold items and reposition them as necessary.  Note that a
    ; panel can contain other panels.
    (define panel%
      (class intermediate-panel% args
	(inherit
	  object-ID
	  get-width
	  get-height
	  get-client-size
	  get-parent)
	
	(public
	  
	  ; list of panel's contents.
	  [children null]
	  
	  ; list of child-info structs corresponding to the children.  (#f
	  ;  if no longer valid.)
	  [children-info null]
	  
	  ; cache to prevent on-size from recomputing its result every
	  ; time. when curr-width is #f, cache invalid.
	  curr-width
	  curr-height
	  
	  ; add-child: adds an existing child to the panel.
	  ; input: new-child: item% descendant to add
	  ;        adder-function: a function which takes a item% and a
	  ;          list of item%s and returns a list of item%s.
	  ; returns: nothing
	  ; effects: sets children to list returned by adder-function
	  ;          forces redraw of window.
	  [add-child
	    (lambda (new-child adder-function)
	      (unless (memq new-child children)
		(set! children (adder-function new-child children))
		(send new-child show #t)
		(force-redraw)))]
	  
	  ; delete-child: removes a child from the panel.
	  ; input: child: child to delete.
	  ; returns: nothing
	  ; effects: removes child from list; forces redraw.
	  [delete-child
	    (lambda (child)
	      (send child show #f)
	      (add-child child remq))]
	  
	  ; get-children-info: returns children info list, recomputing it
	  ;   if needed.
	  ; input: none
	  ; returns: list of child-info structs.
	  ; effects: upon exit, children-info is eq? to result.
	  [get-children-info
	    (lambda ()
	      (printf "Entering get-children-info; object ~s~n" object-ID)
	      (unless children-info
		(printf "Recomputing children info~n")
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
	      (printf "Entering force-redraw; object ~s~n" object-ID)
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
				   (+ default-spacing
				      (child-info-x-posn curr-info)
				      (child-info-x-min curr-info)))
				 (max y-accum
				   (+ default-spacing
				      (child-info-y-posn curr-info)
				      (child-info-y-min
					curr-info)))))))])
	      (lambda ()
		(gms-helper (get-children-info)
		  default-spacing default-spacing)))]
	  
	  ; on-size: called when the container is resized (usu by its
          ;   parent) 
	  ; input: new-width/new-height: new size of panel
	  ; returns: nothing
	  ; effects: causes children to redraw themselves.
	  [on-size
	    (lambda (new-width new-height)
	      (printf "Entering on-size; object ID ~s;  " object-ID)
	      (printf "New size: ~s x ~s~n" new-width new-height)
	      (let-values ([(client-width client-height)
			    (get-two-int-results get-client-size)])
		(printf "Client size: ~s x ~s~n" client-width client-height)
		(unless (and (number? curr-width)
			  (number? curr-height)
			  (= client-width curr-width)
			  (= client-height curr-height))
		  (set! curr-width client-width)
		  (set! curr-height client-height)
		  (printf "On-size is forcing a redraw.~n")
		  (redraw client-width client-height))))]
	  
	  ; redraw: redraws panel and all children
	  ; input: width, height: size of drawable area in panel.
	  ; returns: nothing
	  ; effects: places children at default positions in panel.
	  [redraw
	    (letrec ([redraw-helper
		       (lambda (children children-info)
			 (unless (null? children)
			   (let ([curr-child (car children)]
				 [curr-info (car children-info)])
			     (send curr-child set-size
			       (child-info-x-posn curr-info)
			       (child-info-y-posn curr-info)
			       (child-info-x-min curr-info)
			       (child-info-y-min curr-info))
			     (redraw-helper (cdr children)
			       (cdr children-info)))))])
	      (lambda (width height)
		(redraw-helper children (get-children-info))))])
	(sequence
	  (apply super-init args))))
    
    (define make-top-container%
      (lambda (base%)
	(class base% args
	  (inherit
	    set-size
	    get-width
	    get-height
	    get-client-size)
	  (public
	    
	    object-ID
	    
	    ; pointer to panel in the frame for use in on-size
	    [panel null]
	    
	    ; insert-panel: update above pointer.
	    ; input: new-panel: panel in frame (descendant of
	    ;   panel%) 
	    ; returns: nothing
	    ; effects: sets panel to new-panel
	    ;          if new-panel is not a descendant of
	    ;            panel%, calls error; panel not updated.
	    [insert-panel
	      (lambda (new-panel)
		(if (is-a? new-panel panel%)
		    (set! panel new-panel)
		    (error 'insert-panel
		      "Expected a mred:panel% descendant; got ~s"
		      new-panel)))]
	    
	    [force-redraw
	      (lambda ()
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
		(printf "FRAME: Entered frame's on-size; args ~s ~s~n"
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
		      (printf "FRAME: panel client ~s x ~s~n"
			p-client-width p-client-height)
		      (printf "FRAME: size differences: ~s, ~s~n"
			delta-w delta-h)
		      (printf "FRAME: New size: ~s x ~s~n"
			new-w new-h)
		      (send panel set-size -1 -1
			(+ (- f-width delta-w) p-delta-w)
			(+ (- f-height delta-h) p-delta-h))
		      (unless (and (= new-width f-width)
				(= new-height f-height))
			(printf "FRAME: Resizing to ~s x ~s~n" f-width
			  f-height)
			(flush-output-port)
			(set-size -1 -1 f-width f-height)))))
		(printf "FRAME: Leaving onsize at the end.~n"))])
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
    ;          location, the inter-object spacing, and the child-info
    ;          struct corresponding to the current object, and return the
    ;          new x/y locations.
    ;        spacing: (defaults to default-spacing).  The size of the gap
    ;          between adjacent objects and objects and the edge of the
    ;          panel.
    ; returns: a thunk which returns the minimum possible size of the panel as
    ;  a list of two elements: (min-x min-y).
    (define make-get-size
      (opt-lambda (container compute-x compute-y
		    [spacing default-spacing])
	(letrec ([gs-help
		   (lambda (kid-info x-accum y-accum)
		     (if (null? kid-info)
			 (begin
			   (printf "Result: ~s~n" (list x-accum y-accum))
			   (list x-accum y-accum))
			 (let ([curr-info (car kid-info)])
			   (gs-help (cdr kid-info)
			     (compute-x x-accum spacing curr-info)
			     (compute-y y-accum spacing curr-info)))))])
	  (lambda ()
	    (printf "Entering get-min-size; object ~s~n" (ivar container
							   object-ID))
	    (gs-help (send container get-children-info)
	      spacing spacing)))))
    
    ; make-h-v-redraw: creates functions suitable for use as the redraw
    ; function in a horizontal-panel% or vertical-panel% class.
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
    ; returns: a function which takes the width and the height of the panel
    ;   and resizes and moves its children appropriately: centered or
    ;   stretched along the minor dimension, and equally spaced along the
    ;   major dimension.
    (define make-h-v-redraw
      (opt-lambda (container child-major-size
		    child-major-stretch
		    child-minor-size
		    child-minor-stretch
		    major-dim minor-dim
		    get-h-info get-v-info
		    [spacing default-spacing])
	(lambda (width height)
	  (letrec ([compute-extra-space
		     (lambda (width kid-info)
		       (if (null? kid-info)
			   (- width spacing)
			   (compute-extra-space
			     (- width spacing
			       (child-major-size (car kid-info)))
			     (cdr kid-info))))]
		   [count-stretchable
		     (lambda (kid-info)
		       (if (null? kid-info)
			   0
			   (let ([curr-info (car kid-info)])
			     (if (child-major-stretch curr-info)
				 (add1 (count-stretchable (cdr kid-info)))
				 (count-stretchable (cdr kid-info))))))]
		   [move-children
		     (lambda (kids x-info y-info)
		       (unless (null? kids)
			 (let ([curr-x (car x-info)]
			       [curr-y (car y-info)])
			   (send (car kids) set-size
			     (car curr-x)
			     (car curr-y)
			     (cadr curr-x)
			     (cadr curr-y))
			   (move-children (cdr kids)
			     (cdr x-info) (cdr y-info)))))])
	    (let* ([get-minor-info
		     (letrec
		       ([get-info
			  (lambda (kid-info)
			    (if (null? kid-info)
				null
				(let ([curr-info (car kid-info)])
				  (cons
				    (list
				      (if (child-minor-stretch curr-info) 
					  spacing
					  (- (inexact->exact
					       (round
						 (/ (minor-dim width
						      height)
						    2)))
					     (inexact->exact
					       (round
					     (/ (child-minor-size
						  curr-info)
						2)))))
				      (if (child-minor-stretch curr-info)
					  (- (minor-dim width height)
					     (* 2 spacing))
					  (child-minor-size curr-info)))
				    (get-info (cdr kid-info))))))])
		       (lambda (kid-info)
			 (get-info kid-info)))]
		   [kid-info (send container get-children-info)]
		   [num-stretchable (count-stretchable kid-info)]
		   [extra-space (compute-extra-space (major-dim width
						       height)
				  kid-info)]
		   [extra-per-stretchable (if (zero? num-stretchable)
					      0
					      (inexact->exact
						(round
						  (/ extra-space
						     num-stretchable))))]
		   [num-children (length kid-info)]
		   [get-major-info
		     (letrec
		       ([get-info
			  (lambda (kid-info left-edge)
			    (if (null? kid-info)
				null
				(let* ([curr-info (car kid-info)]
				       [posn (+ left-edge spacing)]
				       [size
					 (if (child-major-stretch
					       curr-info)
					     (+ extra-per-stretchable
						(child-major-size
						  curr-info))
					     (child-major-size
					       curr-info))])
				  (cons (list posn size)
				    (get-info (cdr kid-info)
				      (+ size posn))))))])
		       (lambda (kid-info) (get-info kid-info 0)))])
	      (let ([major-info (get-major-info kid-info)]
		    [minor-info (get-minor-info kid-info)])
		(printf "Inside redraw; object ~s;  "
		  (ivar container object-ID))
		(printf "Arguments: ~s ~s~n" width height)
		(printf "# stretchable: ~s;  Extra space: ~s;  "
		  num-stretchable extra-space)
		(printf "space / stretchable: ~s~nMajor Info: ~s~n"
		  extra-per-stretchable major-info)
		(printf "minor info: ~s~n" minor-info)
		(move-children (ivar container children)
		  (get-h-info major-info minor-info)
		  (get-v-info major-info minor-info))))))))
    
    ; horizontal-panel%: a panel which arranges its children in an evenly
    ; spaced horizontal row.  Items are vertically centered (or stretched
    ; to fit the dialog box if they are stretchable).  The items are evenly
    ; spaced horizontally, with any extra space divided evenly among the
    ; stretchable items. 
    (define horizontal-panel%
      (class-asi panel%
	(public
	  [get-min-size
	    (make-get-size this
	      (lambda (curr-x spacing curr-info)
		(+ curr-x spacing (child-info-x-min curr-info)))
	      (lambda (curr-y spacing curr-info)
		(max curr-y
		  (+ (* 2 spacing)
		     (child-info-y-min curr-info)))))]
	  [redraw
	    (make-h-v-redraw this
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
	  [get-min-size
	    (make-get-size this
	      (lambda (curr-x spacing curr-info)
		(max curr-x (+ (* 2 spacing)
			       (child-info-x-min curr-info))))
	      (lambda (curr-y spacing curr-info)
		(+ curr-y spacing (child-info-y-min curr-info))))]
	  [redraw
	    (make-h-v-redraw this
	      child-info-y-min
	      child-info-y-stretch
	      child-info-x-min
	      child-info-x-stretch
	      (lambda (width height) height)
	      (lambda (width height) width)
	      (lambda (major minor) minor)
	      (lambda (major minor) major))])))
    
    (define add-at-end
      (lambda (object list-of-kids)
	(append list-of-kids (list object))))
    
    ; implement a panel which can hold multiple objects but only displays
    ; one at a time.  The size of the panel is the smallest size possible
    ; for displaying each of the panel's children.
    (define single-panel%
      (class-asi panel%
	(public
	  
	  ; pointer to currently active child
	  [active null]
	  
	  [add-child
	    (lambda (new-child adder-function)
	      (unless (memq new-child children)
		(set! children (adder-function new-child children))
		(send new-child show #f)
		(force-redraw)))]
	  [delete-child
	    (lambda (child)
	      (send child show #f)
	      (add-child child remq))]
	  [active-child
	    (case-lambda
	      [() active]
	      [(new-child)
	       (unless (memq new-child children)
		 (add-child new-child add-at-end))
	       (send active show #f)
	       (send new-child show #t)
	       (set! active new-child)])]
	  [get-min-size
	    (make-get-size this
	      (lambda (curr-x spacing info)
		(max curr-x (child-info-x-min info)))
	      (lambda (curr-y spacing info)
		(max curr-y (child-info-y-min info)))
	      0)]
	  [redraw
	    (lambda (width height)
	      (unless (null? active)
		(let* ([active-info (send active get-info)]
		       [x-stretch (child-info-x-stretch active-info)]
		       [x-min (child-info-x-min active-info)]
		       [y-stretch (child-info-y-stretch active-info)]
		       [y-min (child-info-y-min active-info)]
		       [x-posn (if x-stretch
				   0
				   (/ (- width x-min) 2))]
		       [x-size (if x-stretch
				   width
				   x-min)]
		       [y-posn (if y-stretch
				   0
				   (/ (- height y-min) 2))]
		       [y-size (if y-stretch
				   height
				   y-min)])
		  (send active set-size x-posn y-posn
		    x-size y-size))))])))))
                                                                                