(unit/sig framework:split-panel^
  (import mred^)

  (define (refresh-panel panel)
    (let-values ([(ha va) (send panel get-alignment)])
      (send panel set-alignment ha va)))

  (define thumb-canvas%
    (class canvas% (parent get-top-min get-bot-min)
      (private
	[percentage 1/2])
      (public
	[get-percentage (lambda () percentage)]
	[set-percentage (lambda (_p) 
			  (set! percentage _p)
			  (on-paint))])
      (private
	[gray-region 18]
	[canvas-width 18]
	[thumb-height 16]
	[thumb-min 16])
      
      (private
	[grabbed? #f]
	[get-thumb-middle
	 (lambda ()
	   (let-values ([(w h) (get-client-size)])
	     (floor (* h percentage))))]
	[get-thumb-top
	 (lambda ()
	   (- (get-thumb-middle) (/ thumb-height 2)))]
	[get-thumb-bottom
	 (lambda ()
	   (+ (get-thumb-top) thumb-height))]
	[update-percentage/draw
	 (lambda (mouse-evt)
	   (let-values ([(w h) (get-client-size)])
	     (let* ([y (inexact->exact (send mouse-evt get-y))]
		    [y-min (max thumb-min (get-top-min))]
		    [y-max (- h (max thumb-min (get-bot-min)))])
	       (set! percentage (/ (min (max y-min y) y-max) h))
	       (on-paint))))])
      (inherit get-dc get-client-size)
      (rename [super-on-event on-event])
      (override
       [on-event
	(lambda (evt)
	  (cond
	   [(send evt button-down?)
	    (set! grabbed? #t)
	    (update-percentage/draw evt)]
	   [(and grabbed? (send evt button-up?))
	    (set! grabbed? #f)
	    (update-percentage/draw evt)
	    (refresh-panel parent)]
	   [(and grabbed? (send evt moving?))
	    (update-percentage/draw evt)]
	   [else (super-on-event evt)]))]
       [on-paint
	(lambda ()
	  (let ([dc (get-dc)]
		[panel-color (get-panel-background)])
	    (let-values ([(w h) (get-client-size)])
	      (send dc set-pen (send the-pen-list find-or-create-pen panel-color 1 'solid))
	      (send dc set-brush (send the-brush-list find-or-create-brush panel-color 'solid))
	      (send dc draw-rectangle 0 0 w h)
	      
	      (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
	      (if grabbed?
		  (send dc set-brush (send the-brush-list find-or-create-brush "blue" 'solid))
		  (send dc set-brush (send the-brush-list find-or-create-brush "black" 'solid)))
	      (send dc draw-polygon
		    (list (make-object point% 2 (get-thumb-middle))
			  (make-object point% (- w 1) (get-thumb-top))
			  (make-object point% (- w 1) (get-thumb-bottom)))))))])
      
      (inherit min-width stretchable-width)
      (sequence 
	(super-init parent)
	(min-width canvas-width)
	(stretchable-width #f))))

  (define (resizable-vertical-mixin super%)
    (class super% args
      
      (override
       [container-size
	(lambda (_lst)
	  ;; remove the thumb canvas from the computation
	  (let ([lst (if (null? _lst) null (cdr _lst))])
	    (values
	     (apply + (map car lst))
	     (if (null? lst) 
		 0
		 (apply max (map cadr lst))))))]
       [place-children
	(lambda (info width height)
	  (let* ([percentage (send thumb-canvas get-percentage)]
		 [first (floor (* percentage height))]
		 [second (- height first)]
		 [main-width (- width (send thumb-canvas min-width))])
	    (list* (list (- width (send thumb-canvas min-width)) 0
			 (send thumb-canvas min-width)
			 height)
		   (list 0 0 main-width first)
		   (list 0 first main-width second)
		   (map (lambda (x) (list 0 0 0 0)) (cdddr info)))))])
      (inherit reflow-container get-top-level-window set-alignment get-alignment)
      (public
	[set-percentage
	 (lambda (p)
	   (send thumb-canvas set-percentage p)
	   (refresh-panel this))])
      
      (sequence
	(apply super-init args))
      (inherit get-children)
      (private
	[make-get-min
	 (lambda (index)
	   (lambda ()
	     (let* ([children (get-children)])
	       (if (< index (length children))
		   (send (list-ref children index) min-height)
		   0))))]
	[thumb-canvas (make-object thumb-canvas% this (make-get-min 2) (make-get-min 3))])))

  (define resizable-vertical-panel% (resizable-vertical-mixin panel%))
  (define resizable-vertical-pane% (resizable-vertical-mixin pane%)))
