(define mred:find-string@
  (unit/sig mred:find-string^
    (import [mred:debug : mred:debug^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:edit : mred:edit^]
	    [mred:frame : mred:frame^])
	    
    (mred:debug:printf 'invoke "mred:find-string@")

    (define make-find-frame%
      (lambda (super%) 
	(class super% (canvas [in-edit ()] [x -1] [y -1] [flags ()])
	  (inherit set-size show center make-modal
		   capture-mouse release-mouse panel)
	  (public
	    [HEIGHT 100]
	    [canvas% mred:canvas:editor-canvas%]
	    [edit% mred:edit:edit%])
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
	       (send panel change-children 
		     (lambda (l) (if on? 
				     (list find-panel replace-panel bottom-panel)
				     (list find-panel bottom-panel)))))]
	    
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
	       (when (string=? (send edit get-text
				     (send edit get-start-position)
				     (send edit get-end-position))
			       (send find-edit get-text))
		 (send edit insert (send replace-edit get-text))))]
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
		  (send edit end-edit-sequence))))])
	  (sequence
	    (super-init () "Find String" x y))
	  
	  (private
	    [find-panel (make-object mred:container:vertical-panel% panel)]
	    [replace-panel (make-object mred:container:vertical-panel% panel)]
	    [bottom-panel (make-object mred:container:horizontal-panel% panel)]
	    [find-message-item (make-object mred:container:message% find-panel "Find:")])
	  
	  (sequence
	    (send bottom-panel stretchable-in-y #f)
	    (send find-panel spacing 0)
	    (send replace-panel spacing 0))

	  (private
	    [allow-return-check-box (make-object mred:container:check-box%
						 bottom-panel on-return-check
						 "Carriage Returns in Search")])
	  (sequence
	    (make-object mred:container:vertical-panel% bottom-panel))
	  (private
	    [done-button (make-object mred:container:button%
				      bottom-panel on-done "OK")])
	  (private
	    [find-canvas (make-object mred:canvas:editor-canvas% find-panel)]
	    [find-edit (send find-canvas get-media)])
	  (sequence
	    (send find-canvas user-min-height HEIGHT)
	    (finder-keymap find-edit))
	  (private
	    [middle-panel (make-object mred:container:horizontal-panel% find-panel)])
	    
	  (sequence 
	    (send middle-panel stretchable-in-y #f)
	    (make-object mred:container:horizontal-panel% middle-panel))
	  
	  (private
	    [find-button (make-object mred:container:button%
				      middle-panel on-find
				      "Find")]
	    [backwards-check-box (make-object mred:container:check-box%
					      middle-panel on-backwards-check
					      "Reverse")]
	    [wrap-check-box (make-object mred:container:check-box%
					 middle-panel on-wrap-check
					 "Wraparound")]
	    [ignore-case-check-box (make-object mred:container:check-box%
						middle-panel on-ignore-case-check
						"Ignore Case")]
	    [replace-check-box (make-object mred:container:check-box%
					    middle-panel on-replace-check
					    "Replace...")])
	  (private
	    [replace-message-item (make-object mred:container:message% replace-panel "Replace:")]
	    [replace-canvas (make-object mred:canvas:editor-canvas% replace-panel)]
	    [replace-edit (send replace-canvas get-media)])
	  (sequence
	    (send replace-canvas user-min-height HEIGHT)
	    (finder-keymap replace-edit))
	  (private
	    [replace-button-panel (make-object mred:container:horizontal-panel% replace-panel)])

	  (sequence 
	    (make-object mred:container:vertical-panel% replace-button-panel)
	    (send replace-button-panel stretchable-in-y #f))

	  (private
	    [find-button-2 (make-object mred:container:button%
					replace-button-panel on-find
					"Find")]
	    [replace-button (make-object mred:container:button%
					 replace-button-panel on-replace
					 "Replace")]
	    [replace-and-find-button (make-object mred:container:button%
						  replace-button-panel on-replace-and-find
						  "Replace and Find Again")]
	    [replace-all-button (make-object mred:container:button%
					     replace-button-panel on-replace-all
					     "Replace All")])
	  (sequence
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

    (define find-frame% (make-find-frame% mred:frame:empty-frame%))

    (define make-searchable-frame%
      (lambda (super%)
	(class super% args
	  (inherit panel active-edit active-canvas)
	  (private
	    [search-edit
	     (make-object
	      (class-asi mred:edit:edit%
		(rename [super-after-insert after-insert]
			[super-after-delete after-delete]
			[super-on-focus on-focus])
		(public
		  [on-focus
		   (lambda (on?)
		     (when on?
		       (set! anchor (send (active-edit) get-end-position)))
		     (super-on-focus on?))]
		  [after-insert
		   (lambda args
		     (apply super-after-insert args)
		     (search #f))]
		  [after-delete
		   (lambda args
		     (apply super-after-delete args)
		     (search #f))])))])
	  (sequence
	    (apply super-init args))
	  (private
	    [search-panel (make-object (class-asi mred:container:horizontal-panel%
					 (rename [super-on-set-focus on-set-focus]) 
					 (public
					   [on-set-focus
					    (lambda ()
					      (or (send media-canvas set-focus)
						  (super-on-set-focus)))]))
				       panel)])
	  (public
	    [hide-search
	     (lambda ()
	       (send panel delete-child search-panel)
	       (send (active-canvas) set-focus)
	       (set! hidden? #t))])
	  (private
	    [media-canvas (make-object mred:canvas:editor-canvas% search-panel
				       -1 -1 -1 -1 "" 
				       (bitwise-ior wx:const-mcanvas-hide-h-scroll
						    wx:const-mcanvas-hide-v-scroll))]
	    [search-button (make-object mred:container:button% search-panel 
					(lambda args (search)) "Search")]
	    [close-button (make-object mred:container:button% search-panel 
				       (lambda args (hide-search)) "Close")]
	    [hidden? #f]
	    [anchor 0])
	  (sequence
	    (send search-panel stretchable-in-y #f)
	    (send search-panel border 1)
	    (send media-canvas set-media search-edit)
	    (send search-edit add-canvas media-canvas)
	    (send media-canvas user-min-height 40)
	    (hide-search))
	  (public
	    [search
	     (opt-lambda ([reset-anchor? #t])
	       (if hidden?
		   (begin (set! hidden? #f)
			  (send panel add-child search-panel)
			  (send search-panel set-focus))
		   (let* ([edit (active-edit)]
			  [string (send search-edit get-text)])
		     (when reset-anchor?
		       (set! anchor (send edit get-end-position)))
		     (let ([first-pos (send edit find-string string 1 anchor)])
		       (cond
			 [(= -1 first-pos)
			  (let ([pos (send edit find-string string 1 0)])
			    (if (= -1 pos)
				(begin (send edit set-position anchor)
				       (wx:bell))
				(send edit set-position pos (+ pos (string-length string)))))]
			 [else 
			  (send edit set-position first-pos
				(+ first-pos (string-length string)))])))))]))))

    (define find-string
      (lambda (canvas in-edit x y flags)
	(cond
	  [(not (member 'replace flags)) 
	   (send (let loop ([p canvas]) 
		   (if (is-a? p wx:frame%)
		       p
		       (loop (send p get-parent))))
	    search)]
	  [else (make-object find-frame% canvas in-edit x y flags)])))))
