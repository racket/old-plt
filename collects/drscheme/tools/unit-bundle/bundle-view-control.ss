#|

-- 

A bundle-table-frame% provides the view of a bundle-table%

It's single initialization is argument is a bundle-table%. It supports:

   extend : ((symbol bundle-manager%) -> void)
   
      called when the bundle-table is extended

   delete : (symbol -> void)
   
      called when an entry in the bundle-table is removed

      
--
      
bundle-pasteboard%

leaf-bundle-snip%
node-bundle-snip%
    
|#

(unit/sig drscheme:bundle:bundle-view/control^
  (import mred^
          drscheme:bundle:misc^
          drscheme:bundle:bundle-model^)
  


  
  (define bundle-pasteboard%
    (class pasteboard% (frame bundle-manager)
      (private
        [contents-snip #f])
      (public
        [get-bundle-manager (lambda () bundle-manager)]
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


      (inherit find-first-snip)
      (rename [super-on-focus on-focus])
      (override
	[on-focus
	 (lambda (on?)
           (unless (find-first-snip)
             (send frame enable-children on?))
	   (super-on-focus on?))]
        [after-select
         (lambda (snip on?)
           (super-after-select snip on?)
           (cond
             [(not on?)
              (send frame enable-children #f)]
             [(is-a? snip node-bundle-snip%)
              (send frame enable-children #t)]
             [else (void)]))])

      (inherit invalidate-bitmap-cache)
      (rename [super-after-select after-select])
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
  
  (define node-bundle-snipclass (make-object snip-class%))
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
      (inherit set-snipclass)
      (sequence
        (super-init)
	(set-snipclass node-bundle-snipclass))))
  
  (define bundle-table-frame%
    (class frame% (bundle-table)
      (public
        [enable-children
         (lambda (x)
           '(send new-leaf-button enable x)
           '(send new-node-button enable x))])
      (sequence
        (super-init "Bundles" #f 400 400))
      (private
        [button-panel (make-object horizontal-panel% this)]
        [text (make-object text%)]
        [new-bundle-button (make-object button%
                             "New Bundle"
                             button-panel
                             (lambda x
                               (new-bundle)))]
        [new-node-button (make-object button% "New Node" button-panel (lambda x (new-node)))]
        [new-leaf-button (make-object button% "New Leaf" button-panel (lambda x (new-leaf)))]
        [canvas (make-object editor-canvas% this text)]
        
	[new-child
	 (lambda (make-child)
	   (let/ec k
	     (let ([out (lambda () (bell) (k #f))]
		   [snip (send text get-focus-snip)])
	       (unless (is-a? snip editor-snip%)
		 (out))
	       (let ([pb (send snip get-editor)])
		 (unless (is-a? pb bundle-pasteboard%)
		   (out))
		 (let ([snip (send pb find-next-selected-snip #f)])
		   (cond
		    [(not snip)
		     (let ([manager (send pb get-bundle-manager)])
		       (when (send manager get-bundle)
			 (out))
		       (send manager set-bundle (make-child)))]
		    [(and (is-a? snip node-bundle-snip%)
			  (not (send pb find-next-selected-snip snip)))
		     (let ([node-bundle (send snip get-bundle)])
		       (send node-bundle add-child (make-child)))]
		    [else (out)]))))))]
	[new-leaf
	 (lambda () (new-child (lambda () (make-object leaf-bundle% '()))))]
        [new-node
	 (lambda ()
	   (let ([label (get-text-from-user "New Node Bundle"
					    "Label of node bundle")])
	     (when label
	       (new-child (lambda () (make-object node-bundle% (string->symbol label) '()))))))]
        
        [new-bundle
         (lambda ()
           (let ([name (get-text-from-user "New bundle" "Name of new bundle")]
                 [bundle-manager (make-object bundle-manager%)])
             (send text insert name)
             (send bundle-manager create-view this (lambda (snip) (send text insert snip)))
             (send text insert #\newline)))])
      (sequence
        (send button-panel stretchable-height #f)
        (enable-children #f))))

  (define (new-bundle-table-frame)
    (send (make-object bundle-table-frame% (make-object bundle-table%)) show #t)))