
  (unit/sig mred:find-string^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:edit : mred:edit^]
	    [mred:frame : mred:frame^]
	    [mred:keymap : mred:keymap^])
	    
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
