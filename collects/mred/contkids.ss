; need to export:
; const-default-size
; const-default-posn
; const-default-spacing
; const-default-border
; counter
; child-info
; get-two-int-values
; make-item%
; button%
; check-box%
; choice%
; gauge%
; list-box%
; message%
; radio-box%
; slider%
; text%
; multi-text%
; canvas%
; media-canvas%
; text-window%

(define mred:container-children@
  (unit/sig mred:container-children^
    (import [mred:debug : mred:debug^]
	    mred:container-frames^
	    mred:container-panels^)
    
    (mred:debug:printf 'invoke "mred:container-children@")
    
    ; this constant is used by several MrEd primitives to signify the default
    ; size of an object.
    (define const-default-size -1)
    (define const-default-posn -1);; ditto for position.
    
    ; default spacing between items.
    (define const-default-spacing 10)
    
    ; default spacing around edge of panel
    (define const-default-border 10)
    
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

    ; get-two-int-values: a wrapper around functions that need to return
    ;   two results.
    ; input: function: a function which takes two boxes and returns results
    ;          in them.
    ; returns: the contents of the two boxes (as multiple values)
    (define get-two-int-values
      (lambda (function)
	(let ([a (box 0)]
	      [b (box 0)])
	  (function a b)
	  (values (unbox a) (unbox b)))))

    (define non-negative-number?
      (lambda (n)
	(and (real? n) (not (negative? n)))))

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
      (opt-lambda (item% stretch-x? stretch-y? make-default-size)
	(class item% args
	  (inherit
	    get-width
	    get-height
	    get-x
	    get-y
	    get-parent
	    set-size)
	  
	  (public
	    
	    ; make-item-param: makes a getter/setter function which
	    ;   controls a parameter of one of the container items.
	    ; input: init-value: initial value for parameter
	    ;        test: function which takes one argument and returns a
	    ;          boolean indicating whether or not a value is
	    ;          acceptable as a value of the parameter.
	    ;        failure: function which takes one argument (the
	    ;          illegal value); called when the value passed in is
	    ;          illegal. 
	    ; returns: function of zero or 1 args which gets/sets a
	    ;   parameter. 
	    [make-item-param
	      (lambda (init-value test failure)
		(let ([curr-value init-value])
		  (case-lambda
		    [() curr-value]
		    [(new-val) (if (test new-val)
				   (begin
				     (set! curr-value new-val)
				     (force-redraw))
				   (failure new-val))])))]
	    
	    ; a unique numeric ID for the object (for debugging).
	    object-ID
	    
	    ; store minimum size of item.  Since we no longer support label
	    ; changes, this will never change, so we don't have to maintain
            ; a validity indicator.
	    min-width
	    min-height

	    [set-label
	     (lambda args
	       (error 'set-label
		      "This operation is not supported on this class."))]
	    
	    ; default-x: gets/sets default x position.  Errors out if new
	    ; value is not a real number; forces a redraw upon a set.
	    [default-x
	      (make-item-param 0 real?
		(lambda (val)
		  (error 'default-x
		    "Expected a non-negative real; received ~s" val)))]

	    [default-y
	      (make-item-param 0 real?
		(lambda (val)
		  (error 'default-y
		    "Expected a non-negative real; received ~s" val)))]

	    ; gets/sets user's requirement for minimum width.  Errors out
	    ; if new value is not a non-negative real number.  Forces a
	    ; redraw upon a set.
	    [user-min-width
	      (make-item-param 0 non-negative-number?
		(lambda (val)
		  (error 'user-min-width
		    "Expected a non-negative real; received ~s" val)))]

	    ; like user-min-width, but the other direction.
	    [user-min-height
	      (make-item-param 0 non-negative-number?
		(lambda (val)
		  (error 'user-min-height
		    "Expected a non-negative real; received ~s" val)))]
	    
	    ; stretchable-in-x?: gets/sets horiz stretchability property.
	    ; input: either nothing or a boolean.
	    ; returns: (if nothing passed in) current stretchability
            ;   property. 
	    ; effects: (if boolean value passed in) sets H stretchability
	    ;   prop. to specified value.
	    [stretchable-in-x?
	      (make-item-param stretch-x? boolean?
		(lambda (val)
		  (error 'stretchable-in-x?
		    "Expected a boolean; received ~s" val)))]
	    
	    ; stretchable-in-y?: see stretchable-in-x? but substitute "y"
            ;   for "x" and "horizontal" for "vertical".
	    [stretchable-in-y?
	      (make-item-param stretch-y? boolean?
		(lambda (val)
		  (error 'stretchable-in-y?
		    "Expected a boolean; received ~s" val)))]
	    
	    ; get-info: passes necessary info up to parent.
	    ; input: none
	    ; returns: child-info struct containing the info about this
	    ;   item.
	    ; intended to be called by item's parent upon resize.
	    [get-info
	      (lambda ()
		(mred:debug:printf 'container-get-info
		  "Entering get-info; object ~s" object-ID)
		(let* ([min-size (get-min-size)]
		       [result (make-child-info (default-x) (default-y)
						(car min-size) (cadr min-size)
						(stretchable-in-x?)
						(stretchable-in-y?))])
		  (mred:debug:printf 'container-get-info "Result: ~s"
		    result)
		  result))]
	    
	    ; force-redraw: unconditionally trigger redraw.
	    ; input: none
	    ; returns: nothing
	    ; effects: forces the item's parent (if it exists) to redraw
	    ;   itself. This will recompute the min-size cache if it is
	    ;   invalid.
	    [force-redraw
	      (lambda ()
		(mred:debug:printf 'container-force-redraw
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
		(mred:debug:printf 'container-get-min-size
		  "get-min-size; object ~s;  "
		  object-ID)
		(mred:debug:printf 'container-get-min-size
		  "Result:  ~s"
		  (list min-width min-height))
		(list
		  (max min-width (user-min-width))
		  (max min-height (user-min-height))))])
	  (sequence
	    (mred:debug:printf 'container-child-init "Args to super-init: ~s"
	      (apply make-default-size args))
	    (apply super-init (apply make-default-size args))
	    (set! min-width (get-width))
	    (set! min-height (get-height))
	    
	    (set! object-ID counter)
	    (set! counter (add1 counter))
	    (default-x (get-x))
	    (default-y (get-y))
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
			parent)]))))))
    
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
	(list* parent callback label x y const-default-size
	       const-default-size args)))
    
    ; these next definitions descend classes from the children of wx:item%
    ; which can be inserted into panel% objects.
    (define button%
      (make-item% wx:button% #f #f standard-make-default-size))

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
			    (get-two-int-values get-client-size)])
		(let ([delta-w (- (get-width) client-width)]
		      [delta-h (- (get-height) client-height)]
		      [horizontal? (positive? (bitwise-and style
						wx:const-horizontal))])
		  (mred:debug:printf 'container-set-min-sizes
		    "entering gauge set-min-size; args ~s ~s"
		    range style)
		  (mred:debug:printf 'container-set-min-sizes
		    "client size: ~s x ~s"
		    client-width client-height)
		  (mred:debug:printf 'container-set-min-sizes
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
			   (list* parent label range x y const-default-size
				  const-default-size style args))
		    args)])
	    (mred:debug:printf 'container-gauge-init
	      "Args to gauge: ~s" new-args)
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
      (make-item% wx:message% #f #f list))
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
			    (get-two-int-values get-client-size)])
		(let ([full-width (get-width)]
		      [full-height (get-height)]
		      [range (add1 (- max-val min-val))]
		      [horizontal? (positive? (bitwise-and style
						wx:const-horizontal))])
		  (mred:debug:printf 'container-set-min-sizes
		    "Entering slider's set-min-size; args ~s ~s ~s"
		    min-val max-val style)
		  (mred:debug:printf 'container-set-min-sizes
		    "Client size: ~s x ~s"
		    client-w client-h)
		  (mred:debug:printf 'container-set-min-sizes
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
			      (get-two-int-values get-client-size)])
		  (mred:debug:printf 'conatiner-canvas-find-min-size
		    "Entering find-min-size.  client size: ~s x ~s"
		    width height)
		  (mred:debug:printf 'container-canvas-find-min-size
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
    (define text-window% (make-canvas% wx:text-window%))))    