
  (unit/sig mred:find-string^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:edit : mred:edit^]
	    [mred:frame : mred:frame^])
	    
    (mred:debug:printf 'invoke "mred:find-string@")

    (define make-find-frame%
      (lambda (super%) 
	(class super% (canvas [in-edit ()] [x -1] [y -1] [flags ()])
	  (inherit set-size show center)
	  (public
	    [HEIGHT 100]
	    [get-canvas% (lambda () mred:canvas:wrapping-canvas%)]
	    [get-edit% (lambda () mred:edit:edit%)])
	  (private
	    [edit (if (null? in-edit)
		      (send canvas get-media)
		      in-edit)]
	    [allow-carriage-returns #f])

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
	       #t)]
	    
	    [show-replace-panel
	     (lambda (on?)
	       (send panel
		     change-children 
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
		 (let ([text (send replace-edit get-text)]
		       [position (send edit get-start-position)])
		   (send edit insert text)
		   (send edit set-position position (+ position (string-length text))))))]
	    [done-find #f]
	    [on-replace-and-find
	     (lambda args
	       (dynamic-wind (lambda () (send edit begin-edit-sequence))
			     (lambda () 
			       (when done-find (on-replace))
			       (on-find))
			     (lambda () (send edit end-edit-sequence))))]
	    
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
	    [panel (make-object mred:container:vertical-panel% this)]
	    [find-panel (make-object mred:container:vertical-panel% panel)]
	    [replace-panel (make-object mred:container:vertical-panel% panel)]
	    [bottom-panel (make-object mred:container:horizontal-panel% panel)]
	    [find-message-item (make-object mred:container:message% find-panel "Find:")])
	  
	  (sequence
	    (send bottom-panel stretchable-in-y #f))

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
	    [find-canvas (make-object mred:canvas:wrapping-canvas% find-panel)]
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
	    [replace-canvas (make-object mred:canvas:wrapping-canvas% replace-panel)]
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
	    
	    (send find-canvas set-focus)))))    

    (define find-frame% (make-find-frame% mred:container:dialog-box%))

    (define make-searchable-frame%
      (let* ([anchor 0]
	     [searching-direction 1]
	     [old-highlight void]
	     [get-active-embedded-edit
	      (lambda (edit)
		(let loop ([edit edit])
		  (let ([snip (send edit get-focus-snip)])
		    (if (or (null? snip)
			    (not (is-a? snip wx:media-snip%)))
			edit
			(loop (send snip get-this-media))))))]
	     [clear-highlight
	      (lambda ()
		(begin (old-highlight)
		       (set! old-highlight void)))]
	     [reset-anchor
	      (let ([color (make-object wx:colour% "BLUE")])
		(lambda (edit)
		  (old-highlight)
		  (let ([position 
			 (if (= 1 searching-direction)
			     (send edit get-end-position)
			     (send edit get-start-position))])
		    (set! anchor position)
		    (set! old-highlight
			  (send edit highlight-range position position color)))))]
	     [replace-edit (make-object mred:edit:edit%)]
	     [find-edit
	      (make-object
	       (class-asi mred:edit:edit%
		 (inherit get-text)
		 (rename [super-after-insert after-insert]
			 [super-after-delete after-delete])
		 (public
		   [searching-frame #f]
		   [set-searching-frame
		    (lambda (frame)
		      (set! searching-frame frame))]
		   [get-searching-edit
		    (lambda ()
		      (get-active-embedded-edit
		       (send searching-frame get-edit-to-search)))]
		   [search
		    (opt-lambda ([reset-anchor? #t] [beep? #t])
		      (when searching-frame
			(let* ([string (get-text)]
			       [searching-edit (get-searching-edit)]
			       [not-found
				(lambda ()
				  (when beep?
				    (wx:bell))
				  #f)]
			       [found
				(lambda (edit first-pos)
				  (let ([last-pos (+ first-pos (* searching-direction 
								  (string-length string)))])
				    (send* edit 
				      (set-caret-owner null wx:const-focus-display)
				      (set-position
					  (min first-pos last-pos)
					  (max first-pos last-pos)))
				    #t))])
			  (when reset-anchor?
			    (reset-anchor searching-edit))
			  (let-values ([(found-edit first-pos)
					(send searching-edit
					      find-string-embedded
					      string 
					      searching-direction
					      anchor
					      -1 #t #t #t)])
			    (cond
			      [(= -1 first-pos)
			       (let-values ([(found-edit pos)
					     (send searching-edit
						   find-string-embedded
						   string 
						   searching-direction
						   (if (= 1 searching-direction)
						       0
						       (send searching-edit last-position)))])
				 (if (= -1 pos)
				     (begin (send found-edit set-position anchor)
					    (not-found))
				     (found found-edit pos)))]
			      [else
			       (found found-edit first-pos)])))))]
		   [on-focus
		    (lambda (on?)
		      (when on?
			(reset-anchor (get-searching-edit))))]
		   [after-insert
		    (lambda args
		      (apply super-after-insert args)
		      (search #f))]
		   [after-delete
		    (lambda args
		      (apply super-after-delete args)
		      (search #f))])))]
	     [canvas% 
	      (class-asi mred:canvas:one-line-canvas%
		(inherit get-parent frame)
		(rename [super-on-set-focus on-set-focus])
		(public
		  [lines 2]
		  [style-flags wx:const-mcanvas-hide-h-scroll]
		  [on-set-focus
		   (lambda ()
		     (send find-edit set-searching-frame frame)
		     (super-on-set-focus))]))])
	(lambda (super%)
	  (class super% args
	    (inherit active-edit active-canvas get-edit)
	    (rename [super-make-root-panel make-root-panel]
		    [super-on-activate on-activate]
		    [super-on-close on-close])
	    (private
	      [super-root 'unitiaialized-super-root])
	    (public
	      [make-root-panel
	       (lambda (% parent)
		 (let* ([s-root (super-make-root-panel
				 mred:container:vertical-panel%
				 parent)]
			[root (make-object % s-root)])
		   (set! super-root s-root)
		   root))])
	    (sequence
	      (mred:debug:printf 'super-init "before searchable-frame%")
	      (apply super-init args)
	      (mred:debug:printf 'super-init "after searchable-frame%"))
	    (public
	      [on-activate
	       (lambda (on?)
		 (unless hidden?
		   (if on?
		       (reset-anchor (get-edit-to-search))
		       (clear-highlight)))
		 (super-on-activate on?))]
	      [get-edit-to-search
	       (lambda () 
		 (get-edit))]
	      [hide-search
	       (lambda ()
		 (send super-root delete-child search-panel)
		 (clear-highlight)
		 (when '(not (active-canvas))
		   (send super-root set-focus))
		 (set! hidden? #t))]
	      [unhide-search
	       (lambda ()
		 (set! hidden? #f)
		 (send super-root add-child search-panel)
		 (reset-anchor (get-edit-to-search))
		 (send search-panel set-focus))])
	    (public
	      [on-close
	       (lambda ()
		 (and (super-on-close)
		      (begin (let ([close-canvas
				    (lambda (canvas edit)
				      (send edit remove-canvas canvas)
				      (send canvas set-media ()))])
			       (close-canvas find-canvas find-edit)
			       (close-canvas replace-canvas replace-edit))
			     (if (eq? this (ivar find-edit searching-frame))
				 (send find-edit set-searching-frame #f))
			     #t)))]
	      [set-search-direction 
	       (lambda (x) 
		 (set! searching-direction x)
		 (send dir-radio set-selection (if (= x 1) 0 1)))]
	      [replace&search
	       (lambda ()
		 (when (replace)
		   (search)))]
	      [replace-all
	       (lambda ()
		 (let* ( [replacee-edit (get-edit-to-search)]
                         [pos (if (= searching-direction 1)
                                (send replacee-edit get-start-position)
                                (send replacee-edit get-end-position))]
                         [get-pos 
                           (if (= searching-direction 1)
                             (lambda ()
                               (ivar replacee-edit get-end-position))
                             (lambda ()
                               (ivar replacee-edit get-start-position)))]
                         [done? (if (= 1 searching-direction)
                                  <=
                                  >=)])
		   (send* replacee-edit 
		     (begin-edit-sequence)
		     (set-position pos))
		   (when (search)
		     (send replacee-edit set-position pos)
		     (let loop ([last-pos pos])
		       (search searching-direction #f)
		       (let ([current-pos (get-pos)])
			 (if (done? current-pos last-pos)
			     (send replacee-edit set-position last-pos)
			     (begin (replace)
				    (loop current-pos))))))
		   (send replacee-edit end-edit-sequence)))]
	      [replace
	       (lambda ()
		 (let* ([search-text (send find-edit get-text)]
			[replacee-edit (get-edit-to-search)]
			[replacee (send replacee-edit get-text 
					(send replacee-edit get-start-position)
					(send replacee-edit get-end-position))])
		   (if (string=? replacee search-text)
		       (begin (send replacee-edit insert (send replace-edit get-text))
			      #t)
		       #f)))]
	      [search
	       (opt-lambda ([direction searching-direction] [beep? #t])
		 (send find-edit set-searching-frame this)
		 (if hidden?
		     (unhide-search)
		     (begin
		       (set-search-direction direction)
		       (send find-edit search #t beep?))))])
            (private
	      [search-panel (make-object mred:container:horizontal-panel% super-root)]

	      [left-panel (make-object mred:container:vertical-panel% search-panel)]
	      [find-canvas (make-object canvas% left-panel)]
	      [replace-canvas (make-object canvas% left-panel)]

	      [middle-panel (make-object mred:container:horizontal-panel% search-panel)]

	      [middle-left-panel (make-object mred:container:vertical-panel% middle-panel)]
	      [middle-middle-panel (make-object mred:container:vertical-panel% middle-panel)]
	      [middle-right-panel (make-object mred:container:vertical-panel% middle-panel)]

	      [spacing1 (make-object mred:container:horizontal-panel% middle-left-panel)]
	      [spacing2 (make-object mred:container:horizontal-panel% middle-middle-panel)]
	      [search-button (make-object mred:container:button% middle-left-panel 
					  (lambda args (search)) "Search")]

	      [replace&search-button (make-object mred:container:button% middle-middle-panel 
						  (lambda x (replace&search)) "Replace && Search")]
	      [replace-button (make-object mred:container:button% middle-left-panel (lambda x (replace)) "Replace")]
	      [replace-all-button (make-object mred:container:button% middle-middle-panel
					       (lambda x (replace-all)) "Replace All")]
	      [spacing3 (make-object mred:container:horizontal-panel% middle-left-panel)]
	      [spacing4 (make-object mred:container:horizontal-panel% middle-middle-panel)]

	      [dir-radio (make-object mred:container:radio-box% middle-right-panel
				      (lambda (dir-radio evt)
					(let ([forward (if (= 0 (send evt get-command-int))
							   1
							   -1)])
					  (set-search-direction forward)
					  (reset-anchor (get-edit-to-search))))
				      null
				      -1 -1 -1 -1
				      (list "Forward" "Backward"))]
	      [close-button (make-object mred:container:button% middle-right-panel
					 (lambda args (hide-search)) "Close")]
	      [hidden? #f])
	    (sequence
	      (let ([align
		     (lambda (x y)
		       (let ([m (max (send x get-width)
				     (send y get-width))])
			 (send x user-min-width m)
			 (send y user-min-width m)))])
		(align search-button replace-button)
		(align replace&search-button replace-all-button))
	      (send search-panel stretchable-in-y #f)
	      (for-each (lambda (x) (send* x (stretchable-in-y #f) (stretchable-in-x #f)))
			(list middle-panel))
	      (for-each (lambda (x) (send x stretchable-in-y #f))
			(list search-panel left-panel))
	      (send find-canvas set-media find-edit)
	      (send replace-canvas set-media replace-edit) 
	      (send find-edit add-canvas find-canvas)
	      (send replace-edit add-canvas replace-canvas)
	      (hide-search))
	    ))))

    (define find-string
      (lambda (canvas in-edit x y flags)
	(cond
	  ['(not (member 'replace flags))
	   (let ([frame (let loop ([p canvas]) 
			  (if (is-a? p wx:frame%)
			      p
			      (loop (send p get-parent))))])
	     (send frame search (if (member 'reverse flags)
				    -1
				    1)))]
	  [else (make-object find-frame% canvas in-edit x y flags)]))))
