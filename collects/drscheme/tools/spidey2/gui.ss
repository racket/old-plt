
(unit/sig ()
  (import mred^
	  mzlib:core^
	  framework^
	  mzlib:print-convert^
	  [drscheme : drscheme:export^]
          [drscheme:arrow : drscheme:draw-arrow^]
	  [zodiac : zodiac:system^])
  
  (define analyze (require-library "spidey2r.ss" "newspidey"))

  ;; used for clickable locations in the program
  (define can-click-style (make-object style-delta% 'change-weight 'bold))
  (send can-click-style set-delta-foreground "purple")

  ;; used for 'red primitives
  (define red-style (make-object style-delta% 'change-weight 'bold))
  (send red-style set-delta-foreground "red")

  ;; used for 'green primitives
  (define green-style (make-object style-delta% 'change-weight 'bold))
  (send green-style set-delta-foreground "forest green")

  ;; normal style must cancel out can-click-style, red-style, and green-style
  (define normal-style (make-object style-delta% 'change-weight 'normal))
  (send normal-style set-delta-foreground "black")
  
  ;; used for the inserted type boxes
  (define box-style (make-object style-delta%))
  (send box-style set-delta-foreground "purple")

  (define spidey-bitmap
    (drscheme:unit:make-bitmap
     "Analyze2"
     (build-path (collection-path "icons") "mrspidey.bmp")))

  (drscheme:get/extend:extend-definitions-text
   (lambda (super%)
     (class super% args

       (private
	 [analyzed? #f]
	 [get-prims void]
	 [get-loc void]
	 [get-var void]
	 [get-type void]
	 [pp-type void]
	 [parents void]
	 [children void]
	 [has-member? void])

       (rename [super-clear-annotations clear-annotations])
       (override
	[clear-annotations
	 (lambda ()
	   (super-clear-annotations)
	   (remove-analysis))])

       (inherit last-position begin-edit-sequence end-edit-sequence change-style)
       (public
	 [spidey2:run-analysis
	  (lambda ()
	    (define _1 (begin
			 (begin-edit-sequence #f)
			 (clear-annotations)
			 (end-edit-sequence)))
	    (define-values/invoke-unit/sig
	      spidey2^
	      (analyze (zodiac:read
			(gui-utils:read-snips/chars-from-text
			 this 0 (last-position))
			(zodiac:make-location 1 1 0 this)))
	      _
	      (zodiac : zodiac:system^))

	    (set! get-prims _:get-prims)
	    (set! get-loc _:get-loc)
	    (set! get-var _:get-var)
	    (set! get-type _:get-type)
	    (set! pp-type _:pp-type)
	    (set! parents _:parents)
	    (set! children _:children)
	    (set! has-member? _:has-member?)

	    (begin-edit-sequence #f)

	    ;; color clickable positions
	    (let loop ([i (last-position)])
	      (unless (zero? i)
		(when (get-var (- i 1))
		  (change-style can-click-style (- i 1) i))
		(loop (- i 1))))
	    (end-edit-sequence)

	    ;; color primitives
	    ;; this will overwrite the highlighting for
	    ;; the clickable spots in the primitives because
	    ;; each primitive should be clickable (only each
	    ;; first character of the primitives is clickable
	    ;; now)
	    (for-each
	     (lambda (prim)
	       (let ([start (zodiac:location-offset (car prim))]
		     [end (+ (zodiac:location-offset (cadr prim)) 1)]
		     [color (caddr prim)])
		 ;; only color primitives in this 
		 (when (and (eq? this
				 (zodiac:location-file (car prim)))
			    (eq? this
				 (zodiac:location-file (cadr prim))))
		   (case color
		     [(red) (change-style red-style start end)]
		     [(green) (change-style green-style start end)]
		     [else (void)]))))
	     (get-prims))

	    (set! analyzed? #t))])

       (inherit delete)
       (private
	 [inserted-snip-poss null]
	 [arrows null]
         [var-pos->edit-pos
          (lambda (pos)
            (let loop ([poss inserted-snip-poss]
                       [pos pos])
	      (cond
                [(null? poss) pos]
                [(<= (car poss) pos) (loop (cdr poss) (+ pos 1))]
                [else (loop (cdr poss) pos)])))]
         [edit-pos->var-pos
	  (lambda (pos)
	    (let loop ([poss inserted-snip-poss]
		       [pos pos])
	      (cond
                [(null? poss) pos]
                [(<= (car poss) pos) (loop (cdr poss) (- pos 1))]
                [else (loop (cdr poss) pos)])))]
	 [move-poss
	  (lambda (start len add)
	    (let ([update-pos
                   (lambda (x)
                     (if (< x start)
                         x
                         (add x len)))])
              (set! inserted-snip-poss (map update-pos inserted-snip-poss))
              (set! arrows (map (lambda (x)
                                  (cons (update-pos (car x))
                                        (update-pos (cdr x))))
                                arrows))))]
	 [add-inserted-snip
	  (lambda (pos)
	    (move-poss pos 1 +)
	    (set! inserted-snip-poss (cons pos inserted-snip-poss)))]
         [clear-arrows
          (lambda ()
            (set! arrows null)
            (invalidate-bitmap-cache))]
	 [clear-inserted-snips
	  (lambda ()
	    (for-each
	     (lambda (pos)
	       (delete pos (+ pos 1) #f))
	     (function:quicksort inserted-snip-poss >=))
	    (set! inserted-snip-poss null))])
       (inherit get-style-list is-locked?)
       (private
	 [check-remove-analysis
	  (lambda ()
	    (eq? (message-box
		  "Clear Analysis?"
		  "Do you want to invalidate the analysis and remove any boxes and arrows?"
		  (send (get-canvas) get-top-level-window)
		  '(yes-no))
		 'yes))]
	 [analysis-modifing? #f]
	 [remove-analysis
	  (lambda ()
	    (set! analyzed? #f)
	    (begin-edit-sequence #f)
            (clear-arrows)
	    (clear-inserted-snips)
	    (change-style normal-style 0 (last-position))
	    (end-edit-sequence))])
       (rename [super-can-insert? can-insert?]
	       [super-after-insert after-insert]
	       [super-can-delete? can-delete?]
	       [super-after-delete after-delete])
       (override
	[can-insert?
	 (lambda (start len)
	   (and (or (not analyzed?)
		    analysis-modifing?
		    (check-remove-analysis))
		(super-can-insert? start len)))]
	[after-insert
	 (lambda (start len)
	   (when (and analyzed? (not analysis-modifing?))
	     (move-poss start len +)
	     (remove-analysis))
	   (super-after-insert start len))]
	[can-delete?
	 (lambda (start len)
	   (and (or (not analyzed?)
		    analysis-modifing?
		    (check-remove-analysis))
		(super-can-delete? start len)))]
	[after-delete
	 (lambda (start len)
	   (when (and analyzed? (not analysis-modifing?))
	     (move-poss start len -)
	     (remove-analysis))
	   (super-after-delete start len))])

       (inherit dc-location-to-editor-location find-position)
       (private
	 [get-pos
	  (lambda (event)
	    (let*-values ([(event-x event-y)
			   (values (send event get-x)
				   (send event get-y))]
			  [(x y) (dc-location-to-editor-location
				       event-x event-y)])
	      (find-position x y)))])
       (rename [super-on-local-event on-local-event]
               [super-on-paint on-paint])
       (inherit insert get-canvas invalidate-bitmap-cache position-location)
       (override
         [on-paint
          (lambda (before? dc left top right bottom dx dy draw-caret)
            (super-on-paint before? dc left top right bottom dx dy draw-caret)
            (unless (null? arrows)
              (let ([pen (send dc get-pen)]
                    [brush (send dc get-brush)])
                (send dc set-pen (send the-pen-list find-or-create-pen "forest green" 1 'solid))
                (send dc set-brush (send the-brush-list find-or-create-brush "purple" 'solid))
                (for-each (lambda (arrow)
                            (let ([start (car arrow)]
                                  [end (cdr arrow)]
                                  [start-x-left (box 0)]
                                  [start-y-top (box 0)]
                                  [end-x-left (box 0)]
                                  [end-y-top (box 0)]
                                  [start-x-right (box 0)]
                                  [start-y-bot (box 0)]
                                  [end-x-right (box 0)]
                                  [end-y-bot (box 0)]
                                  [avg (lambda (x y) (/ (+ (unbox x) (unbox y)) 2))])
                              (position-location start start-x-left start-y-top #t)
                              (position-location start start-x-right start-y-bot #f)
                              (position-location end end-x-left end-y-top #t)
                              (position-location end end-x-right end-y-bot #f)
                              (drscheme:arrow:draw-arrow dc 
                                                         (avg start-x-left start-x-right)
                                                         (avg start-y-top start-y-bot)
                                                         (avg end-x-left end-x-right) 
                                                         (avg end-y-top end-y-bot)
                                                         dx dy)))
                          arrows)
                (send dc set-pen pen)
                (send dc set-brush brush))))]
         [on-local-event
          (lambda (event)
            (cond
              [(not analyzed?)
               (super-on-local-event event)]
              [(and (send event button-down? 'right)
                    (get-var (edit-pos->var-pos (get-pos event))))
               =>
               (lambda (set-var)
                 (let ([menu (make-object popup-menu%)]
                       [pos (get-pos event)])
                   (make-object menu-item%
                     "Show value set"
                     menu
                     (lambda (item evt)
                       (let ([t (make-object text%)])
                         (send t insert (pp-type (get-type set-var)))
                         (set! analysis-modifing? #t)
                         (begin-edit-sequence #f) ;; so it is not undoable...
                         (insert (make-object editor-snip% t) pos pos)
                         (change-style box-style pos (+ pos 1))
                         (invalidate-bitmap-cache)
                         (end-edit-sequence)
                         (set! analysis-modifing? #f)
                         (add-inserted-snip pos))))
                   
                   (let ([make-children/parents-item
                          (lambda (label children/parents pair)
                            (make-object menu-item%
                              label
                              menu
                              (lambda (item evt)
                                (for-each (lambda (var)
                                            (let ([loc (get-loc var)])
                                              (when loc
                                                (let ([parent/child
                                                       (var-pos->edit-pos
                                                        (zodiac:location-offset
                                                         loc))])
                                                  (set! arrows (cons (pair pos parent/child) arrows))))))
                                          (children/parents set-var))
                                (invalidate-bitmap-cache))))])
                     (make-children/parents-item "Parents" parents (lambda (x y) (cons y x)))
                     (make-children/parents-item "Children" children cons))
                   
                   (send (get-canvas) popup-menu menu
                         (+ 1 (inexact->exact (floor (send event get-x))))
                         (+ 1 (inexact->exact (floor (send event get-y)))))))]
              [else
               (super-on-local-event event)]))])

       (sequence
	 (apply super-init args)))))

  (drscheme:get/extend:extend-unit-frame
   (lambda (super%)
     (class super% args
       (inherit button-panel)
       (sequence (apply super-init args))
       (rename [super-disable-evaluation disable-evaluation]
	       [super-enable-evaluation enable-evaluation])

       (inherit definitions-text)
       (override
	[enable-evaluation
	 (lambda ()
	   (send spidey2:analyze-button enable #t)
	   (super-enable-evaluation))]
	[disable-evaluation
	 (lambda ()
	   (send spidey2:analyze-button enable #f)
	   (super-disable-evaluation))])
       (public
	 [spidey2:analyze-button (make-object button%
				   (spidey-bitmap this)
				   button-panel
				   (lambda (button evt)
				     (send definitions-text spidey2:run-analysis)))])
       (sequence
	 (send button-panel change-children
	       (lambda (l)
		 (cons spidey2:analyze-button (function:remq spidey2:analyze-button l)))))))))
