;; [Robby]

(define-sigfunctor (mred:find-string@ mred:find-string^)
  (import mred:canvas^ mred:edit^ mred:frame^)

  '(printf "mred:find-string@~n")

  (define make-find-frame%
    (lambda (super%) 
      (class super% (canvas [in-edit ()] [x -1] [y -1] [flags ()])
        (inherit set-client-size show center make-modal
		 capture-mouse release-mouse)
	(public
	 [WIDTH 450]
	 [HEIGHT 100]
	 [canvas% mred:canvas^:canvas%]
	 [edit% mred:edit^:edit%])
	(private
	 [edit (if (null? in-edit)
		   (send canvas get-media)
		   in-edit)]
	 [allow-carriage-returns #f]
	 [use-capture? (eq? wx:platform 'unix)])
	
	(public
	 [finder-keymap
	  (lambda (edit)
	    (let ([keymap (send edit get-keymap)])
	      (send keymap add-key-function
		    "return"
		    (lambda args
		      (if allow-carriage-returns
			  #f
			  (on-find))))
	      (send keymap add-key-function
		    "forward"
		    (lambda args
		      (send backwards-check-box set-value #f)
		      (on-find)))
	      (send keymap add-key-function
		    "reverse"
		    (lambda args
		      (send backwards-check-box set-value #t)
		      (on-find)))
	      (send keymap add-key-function
		    "done"
		    (lambda args (on-done)))
	      (send keymap add-key-function
		    "tab"
		    (lambda args
		      (if (send replace-check-box get-value)
			  (if (send find-canvas is-focus-on?)
			      (send replace-canvas set-focus)
			      (send find-button-2 set-focus))
			  (send find-button set-focus))))
	      
	      (send keymap map-function "return" "return")
	      (send keymap map-function "c:s" "forward")
	      (send keymap map-function "c:r" "reverse")
	      (send keymap map-function "c:x;c:c" "done")
	      (send keymap map-function "tab" "tab")))]

	 [on-done
	  (lambda args
	    (on-close)
	    (show #f))]

	 [on-close
	  (lambda ()
	    (send canvas force-display-focus #f)
	    (if use-capture?
		(release-mouse)
		(make-modal #f))
	    #t)]

	 [show-replace-panel
	  (lambda (on?)
	    (if on?
		(begin
		  (send find-button show #f)
		  (set-client-size WIDTH long-height))
		(begin
		  (send find-button show #t)
		  (set-client-size WIDTH short-height))))]
	 
	 [on-replace-check
	  (lambda (button event)
	    (show-replace-panel (send event checked?)))]

	 [on-return-check
	  (lambda (button event)
	    (set! allow-carriage-returns (send event checked?)))]
	 
	 [on-backwards-check
	  (lambda args #f)]
	 [on-wrap-check
	  (lambda args #f)]
	 [on-ignore-case-check
	  (lambda args #f)]

	 [do-find
	  (opt-lambda (f [bell wx:bell]
			 [end (if (send backwards-check-box get-value)
				  0 
				  (send edit last-position))]
			 [wrap-ok #t])
		      (set! done-find #t)
		      (let* ([backwards (send backwards-check-box get-value)]
			     [caseless (send ignore-case-check-box get-value)]
			     [direction (if backwards -1 1)]
			     [start (if backwards
					(send edit get-start-position)
					(send edit get-end-position))]
			     [text (send find-edit get-text)]
			     [sos (not backwards)]
			     [next (send edit find-string text direction 
					 start end
					 sos (not caseless))])
			(if (>= next 0)
			    (f next (+ next (string-length text)))
			    (if (and wrap-ok (send wrap-check-box get-value))
				(let* ([new-start
					(if backwards
					    (send edit last-position)
					    0)]
				       [next (send edit find-string text direction
						   new-start start sos
						   (not caseless))])
				  (if (>= next 0)
				      (f next (+ next (string-length text)))
				      (bell)))
				(bell)))))]



	 [on-find
	  (lambda args
	    (do-find (lambda (start end)
		       (send edit set-position start end))))]
	 
	 [on-replace
	  (lambda args
	    (let ([text (send replace-edit get-text)])
	      (send edit insert text)))]
	 [done-find #f]
	 [on-replace-and-find
	  (lambda args
	    (if done-find (on-replace))
	    (on-find))]

	 [on-replace-all
	  (lambda args
	    (dynamic-wind
	     (lambda ()
	       (send edit begin-edit-sequence))
	     (lambda ()
	       (let* ([backwards (send backwards-check-box get-value)]
		      [dir-check (if backwards > <)]
		      [real-start (send edit get-start-position)])
		 (letrec ([text (send replace-edit get-text)]
			  [repl-&-find
			   (lambda (start end)
			     (send edit set-position start end)
			     (send edit insert text start end)
			     (if (< start
				    real-start)
				 (set! real-start 
				       (+ real-start (string-length text)
					  (- start end))))
			     (if (dir-check start real-start)
				 (do-find repl-&-find (lambda () #t)
					  real-start #f)
				 (do-find repl-&-find (lambda () #t))))])
		   (do-find repl-&-find (lambda () #t)))))
	     (lambda ()
	       (send edit end-edit-sequence))))]

	 [on-size
	  (lambda (w h)
	    (set! WIDTH w)
	    (send top-panel set-size -1 -1 w -1)
	    (send find-canvas set-size -1 -1 w HEIGHT)
	    (send middle-panel set-size -1 -1 w -1)
	    (send middle-panel2 set-size -1 -1 w -1)
	    (send replace-canvas set-size -1 -1 w HEIGHT)
	    (send bottom-panel set-size -1 -1 w -1))])

	
	(sequence
	  (super-init () "Find String" x y))
	
	(private
	 [top-panel (make-object wx:panel% this)]
	 [find-message-item (make-object wx:message% top-panel "Find:")]
	 [allow-return-check-box (make-object wx:check-box%
					      top-panel on-return-check
					      "Carriage Returns in Search")]
	 [done-button (make-object wx:button%
				   top-panel on-done
				   "Done")])
	(sequence
	  (let ([y (send done-button get-y)]
		[w (send done-button get-width)]
		[h (send done-button get-height)])
	    (send done-button set-size
		  (- WIDTH w 20) y
		  w h))
	  
	  (send top-panel fit))
	
	(private
	 [find-canvas
	  (let ([h (send top-panel get-height)])
	    (make-object mred:canvas^:canvas%
			 this 0 h
			 WIDTH HEIGHT))]
	 [find-edit (send find-canvas get-media)])
	(sequence
	  (finder-keymap find-edit))
	(private
	 [middle-panel
	  (let ([h (send find-canvas get-height)]
		[y (send find-canvas get-y)])
	    (make-object wx:panel% this
			 0 (+ y h)))]
	 [find-button (make-object wx:button%
				   middle-panel on-find
				   "Find")]
	 [backwards-check-box (make-object wx:check-box%
					   middle-panel on-backwards-check
					   "Reverse")]
	 [wrap-check-box (make-object wx:check-box%
				      middle-panel on-wrap-check
				      "Wraparound")]
	 [ignore-case-check-box (make-object wx:check-box%
					     middle-panel on-ignore-case-check
					     "Ignore Case")]
	 [replace-check-box (make-object wx:check-box%
					 middle-panel on-replace-check
					 "Replace...")])
	(sequence
	  (send middle-panel new-line)
	  (send middle-panel fit))
	(private
	 [short-height
	  (let ([h (send middle-panel get-height)]
		[y (send middle-panel get-y)])
	    (+ h y))]
	 [middle-panel2 (make-object wx:panel% this
				     0 short-height)]
	 [replace-message-item (make-object wx:message% middle-panel2 
					    "Replace:")])
	(sequence
	  (send middle-panel2 new-line)
	  (send middle-panel2 fit))
	(private
	 [replace-canvas
	  (let ([h (send middle-panel2 get-height)]
		[y (send middle-panel2 get-y)])
	    (make-object mred:canvas^:canvas%
			 this 0 (+ y h)
			 WIDTH HEIGHT))]
	 [replace-edit (send replace-canvas get-media)])
	(sequence
	  (finder-keymap replace-edit))
	(private
	 [bottom-panel
	  (let ([h (send replace-canvas get-height)]
		[y (send replace-canvas get-y)])
	    (make-object wx:panel% this
			 0 (+ y h)))]
	 [find-button-2 (make-object wx:button%
				     bottom-panel on-find
				     "Find")]
	 [replace-button (make-object wx:button%
				      bottom-panel on-replace
				      "Replace")]
	 [replace-and-find-button (make-object wx:button%
					       bottom-panel on-replace-and-find
					       "Replace and Find Again")]
	 [replace-all-button (make-object wx:button%
					  bottom-panel on-replace-all
					  "Replace All")])
	(sequence
	  (send bottom-panel new-line)
	  (send bottom-panel fit))
	(private
	 [long-height (+ (send bottom-panel get-height)
			 (send bottom-panel get-y))])
	(sequence
	  (set-client-size WIDTH short-height)
	  
	  (if (member 'replace flags)
	      (begin
		(send replace-check-box set-value #t)
		(show-replace-panel #t)))
	  (if (member 'reverse flags)
	      (send backwards-check-box set-value #t))
	  (if (member 'ignore-case flags)
	      (send ignore-case-check-box set-value #t))
	  (if (member 'wraparound flags)
	      (send wrap-check-box set-value #t))
	  
	  (center wx:const-both)
	  
	  (show #t)
	  (send canvas force-display-focus #t)
	  
	  ; The problem with make-modal in X is that it does
	  ; not let the current focus-owning Widget to get
	  ; an unfocus event. capture-mouse does what we
	  ; really want in X, but not in Windows
	  (if use-capture?
	      (capture-mouse)
	      (make-modal #t))
	  
	  (send find-canvas set-focus)))))

  (define find-frame% (make-find-frame% mred:frame^:empty-frame%))

;; minibuffer searching
;; wx:media-edit cannot go inside a panel?
;; need a new kind of selection to display the found text.

  (define minibuffer-search-panel%
    (class wx:panel% (parent . args)
      (rename [super-set-size set-size])
      (sequence
	(apply super-init (cons parent args)))
      (private
	[search-callback (lambda (obj evt) (search))]
	[close-callback (lambda (obj evt) (send parent clear-mini-panel%))]
	[search-edit (make-object (class-asi mred:edit^:edit%
				    (rename [super-after-insert after-insert]
					    [super-after-delete after-delete])
				    (public
				      [after-insert
				       (lambda args
					 (apply super-after-insert args)
					 (search #f))]
				      [after-delete
				       (lambda args
					 (apply super-after-delete args)
					 (search #f))])))]
	[media-canvas (make-object mred:canvas^:canvas% this 10 10 100 100
 				   "" (bitwise-ior wx:const-mcanvas-hide-h-scroll
						   wx:const-mcanvas-hide-v-scroll)
				   100 search-edit)]
	[search-button (make-object wx:button% this search-callback "Search")]
	[close-button (make-object wx:button% this close-callback "Close")]

	[anchor 0]

	[button-height 26]
	[spacing 4]
	[vertical-offset 3])
      (public

	;; need the +40 for the media-buffer's extra crap.
	[desired-height (lambda () (+ (max (+ (send search-edit line-location 0 #t) 40)
					   button-height)
				      (* 2 vertical-offset)))]

	[search
	 (opt-lambda ([reset-anchor? #t])
	   (let* ([edit (send parent active-edit)]
		  [string (send search-edit get-text)])
	     (when reset-anchor?
	       (set! anchor (send edit get-end-position)))
	     (let ([first-pos (send edit find-string string 1 anchor)])
	       (cond
		 [(= -1 first-pos)
		  (if reset-anchor?
		      (let ([pos (send edit find-string string 1 0)])
			(if (= -1 pos)
			    (begin (send edit set-position anchor)
				   (wx:bell))
			    (begin (set! anchor 0)
				   (send edit set-position pos 
					 (+ pos (string-length string))))))
		      (begin (send edit set-position anchor)
			     (wx:bell)))]
		 [else 
		  (send edit set-position first-pos
			     (+ first-pos (string-length string)))]))))]
	[on-set-focus
	 (lambda () (send media-canvas set-focus))]
	[set-size
	 (lambda (x y width height)
	   (let* ([search-width 60]
		  [close-width 50]
		  [search-x (- width search-width)]
		  [close-x (- search-x close-width spacing)])
	     (send search-button set-size
		   search-x vertical-offset search-width button-height 0)
	     (send close-button set-size
		   close-x vertical-offset close-width button-height 0)
	     (send media-canvas set-size 0 0 (- close-x spacing) height))
	   (super-set-size x y width height))])))

  (define use-minibuffer? #t)

  (define find-string
    (lambda (canvas in-edit x y flags)
      (cond
       [(and use-minibuffer?
	     (not (member 'replace flags)))
	(let* ([frame (let loop ([p canvas])
			(let ([parent (send p get-parent)])
			  (if (null? parent)
			      p
			      (loop parent))))]
	       [mini-panel (send frame get-mini-panel)])
	  (if mini-panel
	      (send mini-panel search)
	      (send frame set-mini-panel% minibuffer-search-panel%)))]
	[else (make-object find-frame% canvas in-edit x y flags)]))))