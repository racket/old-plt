
(module guibuilder mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   (prefix framework: (lib "framework.ss" "framework"))
	   "utils.ss")

  ;; These modules implement snips for the various
  ;;  kinds of windows and controls.
  (require "base.ss"
	   "panel.ss"
	   "simple-control.ss"
	   "text-field.ss"
	   "multiple-choice.ss"
	   "slider-guage.ss"
	   "canvas.ss")
  
  ;; INVARIANT: If a snip is selected, then no ancestor or
  ;;  decendent of the snip can be selected. Otherwise, the
  ;;  dragging rules get complicated (perhaps impossible).
  
  ;; INVARIANT: a child must be ordered before its parent in the
  ;;  pasteboard. Not only does this affect drawing, but it also
  ;;  affects how select-all and rubber-banding work due to the
  ;;  ancestor/decendent-selection-exclusion rule.
  
  (define START-FRAME-WIDTH 100)
  (define START-FRAME-HEIGHT 100)

  ;; Keep in order of choice items:
  (define FRAME-MODE 0)
  (define DIALOG-MODE 1)
  (define PANEL-MODE 2)

  (define -FIRST-MODE- FRAME-MODE)
  (define -LAST-MODE- PANEL-MODE)

  (define top-font (send mred:the-font-list find-or-create-font 
			 12 'default 'normal 'normal #f))

  (define gb:edit%
    (class framework:pasteboard:info%
      (inherit set-selected find-next-selected-snip insert
	       find-first-snip is-selected? add-selected remove-selected
	       get-admin find-snip begin-edit-sequence end-edit-sequence
	       get-snip-location delete erase set-modified resize
	       invalidate-bitmap-cache
	       begin-write-header-footer-to-file end-write-header-footer-to-file)
      (rename [super-interactive-adjust-move interactive-adjust-move]
	      [super-interactive-adjust-resize interactive-adjust-resize]
	      [super-on-interactive-move on-interactive-move]
	      [super-after-interactive-move after-interactive-move]
	      [super-after-interactive-resize after-interactive-resize]
	      [super-on-default-event on-default-event]
	      [super-after-delete after-delete]
	      [super-after-insert after-insert]
	      [super-do-paste do-paste]
	      [super-do-copy do-copy]
	      [super-write-footers-to-file write-footers-to-file]
	      [super-read-footer-from-file read-footer-from-file])
      (private 
	[dragging? #f]
	[pasting? #f]
	[copying? #f]
	[cur-hilite #f]
	[cur-hilite-pos 0]
	[cur-id 1])
      (public*
	[new-id (lambda () 
		  (begin0 
		    (number->string cur-id)
		    (set! cur-id (add1 cur-id))))]
	[for-each-snip
	 (lambda (f)
	   (let loop ([s (find-first-snip)])
	     (when s
	       (f s)
	       (loop (send s next)))))]
	[for-each-selected-snip
	 (lambda (f)
	   (let loop ([s (find-next-selected-snip #f)])
	     (when s
	       (f s)
	       (loop (find-next-selected-snip s)))))]
	[in-selected-hierarchy?
	 (lambda (s)
	   (or (is-selected? s)
	       (let ([parent (send s gb-get-parent)])
		 (and parent
		      (in-selected-hierarchy? parent)))))]
	[find-unselected-snip
	 (lambda (x y)
	   (let ([s (find-snip x y)])
	     (if (or (not s) (and (not (in-selected-hierarchy? s))
				  (send s container?)))
		 s
		 (let loop ([s (find-first-snip)])
		   (cond
		     [(not s) #f]
		     [(and (send s container?)
			   (not (in-selected-hierarchy? s)))
		      (let ([tb (box 0)]
			    [lb (box 0)]
			    [bb (box 0)]
			    [rb (box 0)])
			(get-snip-location s lb tb #f)
			(get-snip-location s rb bb #t)
			(if (and (<= (unbox lb) x (unbox rb))
				 (<= (unbox tb) y (unbox bb)))
			    s
			    (loop (send s next))))]
		     [else (loop (send s next))])))))]
	[find-snip-by-XXX
	 (lambda (id get)
	   (let/ec found
	     (for-each-snip
	      (lambda (s)
		(when (equal? id (get s))
		  (found s))))
	     #f))]
	[find-snip-by-id
	 (lambda (id)
	   (find-snip-by-XXX id (gb-id)))]
	[find-snip-by-original-id
	 (lambda (id)
	   (find-snip-by-XXX id gb-original-id))]
	[find-snip-by-name
	 (lambda (id)
	   (find-snip-by-XXX id gb-name))]

	[top-resized
	 (lambda (snip old-w old-h w h)
	   (when (eq? snip main-panel)
		 (unless (= top-level-type PANEL-MODE)
			 (invalidate-bitmap-cache 0 0
						  (+ (max old-w w) (* 2 margin))
						  (+ (max old-h h) (* 2 margin)
						     (or frame-label-h 0) 2)))))])

      (override*
	[can-move-to?
	 (lambda (snip x y dragging?)
	   (or (not (eq? snip main-panel))
	       (and (= x main-panel-x)
		    (= y main-panel-y))))]
	[after-move-to
	 (lambda (snip x y dragging?)
	   (when dragging?
	     (send snip gb-drag-children-along x y)))]
	[after-resize
	 (lambda (snip w h did?)
	   (when (and (eq? snip main-panel) did?)
		 (unless (= top-level-type PANEL-MODE)
			 (invalidate-bitmap-cache 
			  0 0 last-frame-paint-w last-frame-paint-h))))]
	[on-interactive-move
	 (lambda (e)
	   (set! dragging? #t)
	   (for-each-snip (lambda (s) (send s gb-set-stable-position)))
	   (super-on-interactive-move e))]
	[on-select
	 (lambda (s on?)
	   (when (and (not copying?) on?)
	     ; deselect parents:
	     (let loop ([p (send s gb-get-parent)])
	       (when p
		 (if (is-selected? p)
		     (remove-selected p)
		     (loop (send p gb-get-parent)))))
	     ; deselect children:
	     (for-each
	      (lambda (c)
		(when (is-selected? c)
		  (remove-selected c)))
	      (send s gb-get-children))))]
	[after-interactive-move
	 (lambda (e)
	   (set! dragging? #f)
	   (super-after-interactive-move e)
	   
	   ; Adjust parent of selected snips & move selected snip's children
	   (for-each-selected-snip
	    (lambda (snip)
	      (when (not (eq? snip main-panel))
		(let* ([parent (send snip gb-get-parent)]
		       [pos (if parent
				(send parent gb-get-child-pos snip)
				-1)])
		  (if cur-hilite
		      (when (or (not (eq? cur-hilite parent))
				(not (= pos cur-hilite-pos)))
			(when parent
			  (send parent gb-remove-child snip))
			(send cur-hilite gb-add-child snip cur-hilite-pos)
			(set! cur-hilite-pos (add1 cur-hilite-pos)))
		      (when parent
			(send parent gb-remove-child snip)
			(send snip gb-install this #f))))
		(send snip gb-need-recalc-size))))
	   
	   (when cur-hilite
	     (send cur-hilite gb-hilite #f)
	     (set! cur-hilite #f)))]
	[interactive-adjust-move
	 (lambda (snip x-box y-box)
	   (super-interactive-adjust-move snip x-box y-box)
	   ;; This doesn't really work very well.
	   '(let ([parent (send snip gb-get-parent)])
	     (when parent
	       (let-values ([(x y w h) 
			     (send (let loop ([p parent])
				     (let ([parent (send p gb-get-parent)])
				       (if parent
					   (loop parent)
					   p)))
				   gb-get-position-and-size)])
		 (when (and (<= x (unbox x-box) (+ x w))
			    (<= y (unbox y-box) (+ y h)))
		   (set-box! x-box (send snip gb-get-stable-x))
		   (set-box! y-box (send snip gb-get-stable-y)))))))]
	[interactive-adjust-resize
	 (lambda (snip wb hb)
	   (super-interactive-adjust-resize snip wb hb)
	   (let-values ([(x-min y-min) (send snip gb-get-saved-min-size)])
	     (when (or (not (gb-x-stretch? snip))
		       (<= (unbox wb) x-min))
	       (set-box! wb x-min))
	     (when (or (not (gb-y-stretch? snip))
		       (<= (unbox hb) y-min))
	       (set-box! hb y-min))))]
	[after-interactive-resize
	 (lambda (snip)
	   (super-after-interactive-resize snip)
	   (send snip gb-need-recalc-size))]
	[on-default-event
	 (lambda (e)
	   (when dragging?
	     (let ([x (send e get-x)]
		   [y (send e get-y)]
		   [xb (box 0)]
		   [yb (box 0)])
	       (send (get-admin) get-dc xb yb)
	       (let ([lx (+ x (unbox xb))]
		     [ly (+ y (unbox yb))])
		 (let ([s (find-unselected-snip lx ly)])
		   (when s
		     (set! cur-hilite-pos (send s gb-find-position lx ly)))
		   (when (and (or cur-hilite s)
			      (not (eq? cur-hilite s)))
		     (begin-edit-sequence)
		     (when cur-hilite
		       (send cur-hilite gb-hilite #f)
		       (set! cur-hilite #f))
		     (when s
		       (set! cur-hilite s)
		       (send s gb-hilite #t))
		     (end-edit-sequence))))))
	   (super-on-default-event e))]
	[on-double-click
	 (lambda (snip e)
	   (send snip gb-open-dialog))]
	[after-delete
	 (lambda (snip)
	   (for-each (lambda (i) (delete i)) (send snip gb-get-children))
	   (let ([parent (send snip gb-get-parent)])
	     (when parent
	       (send parent gb-remove-child snip)))
	   (super-after-delete snip))]
	[can-insert?
	 (lambda (snip before x y)
	   (is-a? snip gb:snip%))]
	[after-insert
	 (lambda (snip behind x y)
	   (super-after-insert snip behind x y)
	   (when pasting?
	     (dynamic-wind
	      (lambda () (set! pasting? #f))
	      (lambda () (send snip gb-install this #f))
	      (lambda () (set! pasting? #t)))))]
	[do-paste
	 (lambda (time)
	   (dynamic-wind
	       (lambda () (set! pasting? #t))
	       (lambda () (super-do-paste time))
	       (lambda () (set! pasting? #f)))
	   (let ([a-paste #f])
	     (for-each-snip
	      (lambda (s)
		(unless a-paste
		  (let ([oi (gb-original-id s)])
		    (when oi
		      (set! a-paste s))))))
	     (handle-new-arrivals)
	     (when a-paste
	       (let ([top-paste (let loop ([a-paste a-paste])
				  (let ([p (send a-paste gb-get-parent)])
				    (if p
					(loop p)
					a-paste)))])
		 (send main-panel gb-add-child top-paste)
		 (set-selected top-paste)))))])
      (public*
	[handle-new-arrivals
	 (lambda ()
	   (let loop ()
	     ((let/ec k
		(for-each-snip 
		 (lambda (s) 
		   (when (send s gb-reconnect-to-original-children)
		     (k loop))))
		void)))
	   (for-each-snip (lambda (s) (send s gb-forget-original-id))))])
      (override*
	[do-copy
	 (lambda (time delete?)
	   (dynamic-wind
	    (lambda () (set! copying? #t))
	    (lambda () 
	      (when (find-next-selected-snip #f)
		(letrec ([selected
			  (let loop ([s (find-next-selected-snip #f)])
			    (let ([next (find-next-selected-snip s)])
			      (if next
				  (cons s (loop next))
				  (list s))))]
			 [close-selected
			  (lambda (method)
			    (lambda (s)
			      (for-each
			       (lambda (child)
				 (method child)
				 ((close-selected method) child))
			       (send s gb-get-children))))])
		  (for-each (close-selected (lambda (x) (add-selected x))) selected)
		  (super-do-copy time delete?)
		  (for-each (close-selected (lambda (x) (remove-selected x))) selected))))
	    (lambda () (set! copying? #f))))])
      (public*
	[get-selected-snip
	 (lambda () 
	   (let ([s (find-next-selected-snip #f)])
	     (if (or (not s)
		     (not (find-next-selected-snip s)))
		 main-panel
		 s)))]
	[insert-element
	 (lambda (c%)
	   (let* ([i (make-object c%)]
		  [se (get-selected-snip)]
		  [s (if (send se container?)
			 se
			 (or (gb-parent se)
			     main-panel))])
	     (send s gb-add-child i)
	     (set-selected s)))])
      (private
       [frame-label "Frame"]
       [frame-label-w #f]
       [frame-label-h #f]
       [last-frame-paint-w 0]
       [last-frame-paint-h 0]
       [main-panel-x 0]
       [main-panel-y 0]
       [margin 2]
       [top-level-type FRAME-MODE]
       [auto-show? #f]
       [configure-frame #f])
      (public*
        [get-top-level-type
	 (lambda () top-level-type)]
	[get-auto-show
	 (lambda () auto-show?)]
	[get-frame-label
	 (lambda () frame-label)]
        [open-dialog
	 (lambda ()
	   (unless configure-frame
	     (set! configure-frame (make-object 
				    (class mred:frame%
				       (rename [super-on-close on-close])
				       (override*
					[on-close
					 (lambda ()
					   (super-on-close)
					   (set! configure-frame #f))])
				       (super-new))
				    "Output"))
	     (let ([p (make-object mred:vertical-panel% configure-frame)])
	       (send p set-alignment 'left 'center)
	       (letrec ([update-frame
			 (lambda ()
			   (send main-panel gb-need-recalc-size)
			   (invalidate-bitmap-cache 0 0 'end 'end))]
			[kind-choice
			 (make-object mred:choice% 
				      "Output:" 
				      '("Frame" "Dialog" "Panel")
				      p
				      (lambda (c e)
					(let ([mode (send c get-selection)])
					  (set! top-level-type mode)
					  (send frame-stuff enable (< mode PANEL-MODE))
					  (update-frame))))]
			[frame-stuff (make-object mred:vertical-panel% p)]
			[title-text (make-one-line/callback-edit 
				     frame-stuff
				     "Frame Title:"
				     (lambda (txt)
				       (unless (string=? frame-label txt)
					       (set! frame-label txt)
					       (let ([w frame-label-w]
						     [h frame-label-h])
						 (set! frame-label-h #f)
						 (update-frame))))
				     frame-label)]
			[auto-show-check (make-object mred:check-box%
						      "Show Frame Automatically" frame-stuff
						      (lambda (c e)
							(set! auto-show? (send c get-value))))])
		 (send frame-stuff set-alignment 'left 'center)
		 (send frame-stuff enable (< top-level-type PANEL-MODE))
		 (send kind-choice stretchable-width #f)
		 (send kind-choice set-selection top-level-type)
		 (send auto-show-check set-value auto-show?))))
	   (send configure-frame show #t))]
        [get-main-location
	 (lambda (snip dc dx dy)
	   (when (eq? snip main-panel)
		 (if (= top-level-type PANEL-MODE)
		     (begin
		       (set! main-panel-x 0)
		       (set! main-panel-y 0))
		     (begin
		       (unless frame-label-h
			 (let-values ([(w h d a) (send dc get-text-extent
						       frame-label top-font)])
			   (set! frame-label-w w)
			   (set! frame-label-h h)))
		       (set! main-panel-x margin)
		       (set! main-panel-y (+ frame-label-h 2 margin))))
		 (set-box! dx main-panel-x)
		 (set-box! dy main-panel-y)))])
      (override*
	[on-paint
	 (lambda (pre? dc l t r b dx dy show-caret?)
	   (unless (or (not pre?) (= top-level-type PANEL-MODE)
		       (not main-panel))
	      (let ([tb (box 0)]
		    [lb (box 0)]
		    [bb (box 0)]
		    [rb (box 0)])
		(get-snip-location main-panel lb tb #f)
		(get-snip-location main-panel rb bb #t)
		(let* ([w (- (unbox rb) (unbox lb))]
		       [h (- (unbox bb) (unbox tb))]
		       [th (+ (or frame-label-h 0) 2)]
		       [tw (+ (* 2 margin) w)]
		       [totalh (+ th (* 2 margin) h)])
		  (when (and (or (<= 0 l tw) (<= 0 r tw) (<= l 0 tw r))
			     (or (<= 0 t totalh) (<= 0 b totalh) (<= t 0 totalh b)))
		    (set! last-frame-paint-w tw)
		    (set! last-frame-paint-h totalh)
		    (send dc draw-rectangle dx dy 
			  tw totalh)
		    (send dc draw-line dx (+ dy th)
			  (+ dx tw -1) (+ dy th))
		    (with-clipping-region 
		     dc (add1 dx) (add1 dy) 
		     (+ tw -2) (- th 2)
		     (lambda ()
		       (let ([f (send dc get-font)])
			 (send dc set-font f)
			 (send dc draw-text frame-label 
			       (+ dx (/ (- tw (or frame-label-w 0)) 2)) 
			       (+ dy 1))
			 (send dc set-font f)))))))))]
	[write-footers-to-file
	 (lambda (stream)
	   (super-write-footers-to-file stream)
	   (let ([out (lambda (name val)
			(let ([info (box 0)])
			  (begin-write-header-footer-to-file stream name info)
			  (send stream << val)
			  (end-write-header-footer-to-file stream (unbox info))))])
	     (out "gb:mode" top-level-type)
	     (out "gb:title" frame-label)
	     (out "gb:show" (if auto-show? 1 0))))]
	[read-footer-from-file
	 (lambda (stream kind)
	   (cond
	     [(string=? kind "gb:mode") 
	      (set! top-level-type 
		    (min -LAST-MODE- 
			 (max -FIRST-MODE- (send stream get-exact))))]
	     [(string=? kind "gb:title") 
	      (set! frame-label (send stream get-string))]
	     [(string=? kind "gb:show") 
	      (set! auto-show? (positive? (send stream get-exact)))]
	     [else (super-read-footer-from-file stream kind)]))])
      (private
	[main-panel #f])
      (public*
	[get-main-panel (lambda () main-panel)]
	[create-main-panel
	 (lambda () 
	   (erase)
	   (set! main-panel (make-object gb:panel-snip%))
	   (insert main-panel 0 0)
	   (resize main-panel START-FRAME-WIDTH START-FRAME-HEIGHT)
	   (send main-panel gb-install this #f)
	   (send main-panel set-id "0")
	   (send main-panel gb-need-recalc-size)
	   (set-modified #f))])
      (override*
	[on-load-file
	 (lambda (file mode)
	   (set! pasting? #t))]
	[after-load-file
	 (lambda (ok?)
	   (set! pasting? #f)
	   (when ok?
	     (set! main-panel (find-snip-by-original-id "0"))
	     (send main-panel set-id "0")
	     (handle-new-arrivals)
	     (set-modified #f)))]
	[get-keymaps (lambda () null)])
      (super-new)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Frame
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-struct tool (icon callback active?))

  (define lg-pen (send mred:the-pen-list find-or-create-pen 
		       (make-object mred:color% 200 200 200) 0 'solid))

  (define dg-pen (send mred:the-pen-list find-or-create-pen 
		       (make-object mred:color% 140 140 140) 0 'solid))

  (define icons (make-hash-table))

  (define toolbar%
    (class mred:canvas%
      (inherit min-height stretchable-height get-dc)
      (private
       [margin 2]
       [icon-size 16]
       [tools null]
       [active-tool #f])
      (private*
       [deactivate-tool
	(lambda ()
	  (when active-tool
		(set-tool-active?! active-tool #f)
		(set! active-tool #f)
		(on-paint)))]
       [activate-tool
	(lambda (mx my only)
	  (let ([y 0]
		[h (+ icon-size (* 2 margin))]
		[w (+ icon-size (* 2 margin))])
	    (let loop ([l tools][x 0])
	      (unless (null? l)
		 (if (and (<= x mx (+ x w)) (<= y my (+ y h))
			  (or (not only) (eq? (car l) only)))
		     (begin
		       (set! active-tool (car l))
		       (set-tool-active?! active-tool #t)
		       (on-paint))
		     (loop (cdr l) (+ x w)))))))])
      (private
       [can-drag #f])
      (override*
       [on-paint
	(lambda ()
	  (let ([dc (get-dc)]
		[y 0]
		[h (+ icon-size (* 2 margin))]
		[w (+ icon-size (* 2 margin))])
	    (let loop ([l tools][x 0])
	      (unless (null? l)
		(let ([tool (car l)])
		  (let ([p (send dc get-pen)]
			[on? (tool-active? tool)])
		    (send dc set-pen (if on? dg-pen lg-pen))
		    (send dc draw-line x y (+ x w -1) y)
		    (send dc draw-line x y x (+ y h -1))
		    (send dc draw-line x (add1 y) (+ x w -2) (add1 y))
		    (send dc draw-line (add1 x) y (add1 x) (+ y h -2))
		    (send dc set-pen (if on? lg-pen dg-pen))
		    (send dc draw-line (+ x 1) (+ y h -1) (+ x w -1) (+ y h -1))
		    (send dc draw-line (+ x w -1) (+ y 1) (+ x w -1) (+ y h -1))
		    (send dc draw-line (+ x 2) (+ y h -2) (+ x w -2) (+ y h -2))
		    (send dc draw-line (+ x w -2) (+ y 2) (+ x w -2) (+ y h -2))
		    (send dc set-pen p))
		  (if (tool-icon tool)
		      (send dc draw-bitmap (tool-icon tool) (+ x margin) margin)
		      (send dc draw-rectangle (+ x margin) margin icon-size icon-size)))
		(loop (cdr l) (+ x w))))))]
       [on-event
	(lambda (e)
	  (cond
	   [(send e button-down?)
	    (deactivate-tool)
	    (activate-tool (send e get-x) (send e get-y) #f)
	    (set! can-drag active-tool)]
	   [(send e button-up?)
	    (set! can-drag #f)
	    (when active-tool
		  (let ([cb (tool-callback active-tool)])
		    (deactivate-tool)
		    (cb #f #f)))]
	   [(send e dragging?)
	    (when can-drag
		  (let ([old-active active-tool])
		    (set! active-tool #f)
		    (activate-tool (send e get-x) (send e get-y) can-drag)
		    (when (and (not active-tool) old-active)
			  (set-tool-active?! old-active #f)
			  (on-paint))))]
	   [else (set! can-drag #f) 
		 (deactivate-tool)]))])
      (public*
       [append-tool
	(lambda (icon-name cb)
	  (let* ([name (string->symbol icon-name)]
		 [icon
		  (hash-table-get 
		   icons name
		   (lambda ()
		     (let* ([icon (make-object mred:bitmap% 
					       (build-path (collection-path "guibuilder") 
							   icon-name))])
		       (if (send icon ok?) 
			   icon
			   #f))))])
	    (hash-table-put! icons name icon)
	    (set! tools (append tools (list (make-tool icon cb #f))))))])
      (super-new)
      (min-height (+ icon-size (* margin 2)))
      (stretchable-height #f)))

  (define my-base-frame% framework:frame:pasteboard-info-file%)

  (define gb:frame%
    (class my-base-frame% 
      (init [file #f])
      (inherit get-editor show get-area-container get-menu-bar)
      (rename [super-help-menu:after-about help-menu:after-about])
      '(override*
        [help-menu:about-string (lambda () "GUI Builder")]
	[help-menu:about
	 (lambda (itm evt)
	   (mred:message-box (format "Version ~a.~a, Copyright (c) 1997, PLT (Matthew Flatt)~n"
				     GB:SNIP-VERSION
				     MINOR-VERSION)
			     "About GUI Builder"))]
	[help-menu:after-about
	 (lambda (help-menu)
	   (make-object mred:menu-item% "GUI Builder Help"
			help-menu
			(lambda (itm evt)
			  (let ([f (make-object framework:frame:text-info-file%
						(build-path (collection-path "guibuilder") 
							    "help.mre"))])
			    (send (send f get-editor) lock #t)
			    (send f show #t))))
	   (super-help-menu:after-about help-menu))])

      (override*	
	[get-editor% (lambda () gb:edit%)])

      (public*
	[instantiate
	 (lambda ()
	   (let ([cl (eval (build-code #t))])
	     (thread
	      (lambda ()
		(parameterize ([mred:current-eventspace (mred:make-eventspace)])
		   (make-object cl))))))]
	[view-source
	 (lambda ()
	   (let ([port (open-output-string)])
	     (pretty-print (build-code #f) port)
	     (let ([f (make-object (framework:frame:text-mixin framework:frame:editor%)
				   "code.scm")])
	       (send (send f get-editor) insert (get-output-string port))
	       (send f show #t))))]
	[build-code
	 (lambda (force-frame?)
	   (let* ([edit (get-editor)]
		  [main (send edit get-main-panel)]
		  [type (send edit get-top-level-type)]
		  [frame-label (if (and (= type PANEL-MODE) force-frame?)
				   "Panel Tester"
				   (send edit get-frame-label))])
	     `(class object% 
		,@(if (and (= type PANEL-MODE) (not force-frame?))
		      '((init top))
		      '())
		,@(cond
		   [(or (= type FRAME-MODE) 
			(and (= type PANEL-MODE) force-frame?))
		    `((public* [get-top% (lambda () frame%)])
		      (field [top (make-object (get-top%) ,frame-label)]))]
		    [(= type PANEL-MODE) null]
		    [else
		     `((public* [get-top% (lambda () dialog%)])
		       (field [top (make-object (get-top%) ,frame-label)]))])
		,@(send main gb-instantiate 'top)
		(super-new)
		,@(if (and (not force-frame?)
			   (or (= type PANEL-MODE) (not (send edit get-auto-show))))
		      null
		      '((send top show #t))))))])
      
      (define toolbar #f)
      (public*
	[init-tools
	 (lambda (mb)
	   (set! toolbar (make-object toolbar% (get-area-container)))
	   (send (get-area-container) change-children
		 (lambda (l)
		   (cons toolbar (remove toolbar l))))
	   
	   (let* ([emenu (make-object mred:menu% "Element" mb)]
		  [append-element-type
		   (lambda (name icon c%)
		     (let ([maker (lambda (i e) (insert-element c%))])
		       (send toolbar append-tool icon maker)
		       (make-object mred:menu-item% name emenu maker)))]
		  [vmenu (make-object mred:menu% "Output" mb)])
	     (make-object mred:menu-item% "Configure Selected" emenu
			  (lambda (i e)
			    (send (get-editor)
				  for-each-selected-snip
				  (lambda (s)
				    (send s gb-open-dialog)))))
	     (make-object mred:separator-menu-item% emenu)
	     (append-element-type "New Vertical Panel" "vpanel.xpm" gb:vertical-panel-snip%)
	     (append-element-type "New Horizontal Panel" "hpanel.xpm" gb:horizontal-panel-snip%)
	     (append-element-type "New Message Label" "message.xpm" gb:message-snip%)
	     (append-element-type "New Button" "button.xpm" gb:button-snip%)
	     (append-element-type "New Checkbox" "checkbox.xpm" gb:check-box-snip%)
	     (append-element-type "New Text Field" "text.xpm" gb:text-snip%)
	     (append-element-type "New List" "list.xpm" gb:list-box-snip%)
	     (append-element-type "New Radiobox" "radiobox.xpm" gb:radio-box-snip%)
	     (append-element-type "New Choice" "choice.xpm" gb:choice-snip%)
	     (append-element-type "New Slider" "slider.xpm" gb:slider-snip%)
	     (append-element-type "New Gauge" "gauge.xpm" gb:gauge-snip%)
	     (append-element-type "New Canvas" "canvas.xpm" gb:canvas-snip%)
	     (append-element-type "New Editor Canvas" "mcanvas.xpm" gb:editor-canvas-snip%)
	     
	     (make-object mred:menu-item% "Configure Output"  vmenu
			  (lambda (i e) (send (get-editor) open-dialog)))
	     (make-object mred:separator-menu-item% vmenu)
	     (make-object mred:menu-item%  "Make Sample Window" vmenu (lambda (i e) (instantiate)))
	     (make-object mred:menu-item%  "Make Source Code" vmenu (lambda (i e) (view-source)))))]
	[insert-element
	 (lambda (c%)
	   (let ([e (get-editor)])
	     (send e insert-element c%)))])

      (super-make-object (or file "GUI Builder"))

      (init-tools (get-menu-bar))
      
      (let ([file (and file (normalize-path file))])
	(if (and file (file-exists? file) (send (get-editor) load-file file))
	    ;; Force title size calc:
	    (let ([e (get-editor)])
	      (send e get-main-location 
		    (send e get-main-panel)
		    (send (send e get-canvas) get-dc)
		    (box 0) (box 0)))
	    (begin
	      (send (get-editor) create-main-panel)
	      (when file
		(send (get-editor) set-filename file)))))
      
      (show #t)))
  
  (framework:handler:insert-format-handler "GUI Builder" "gui"
					   (lambda (file)
					     (make-object gb:frame% file)))
  
  (define (new-gui-builder-frame) (new gb:frame% [height 400]))
					       

  (new-gui-builder-frame))

