
  (unit/sig mred:gui-utils^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:frame : mred:frame^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:edit : mred:edit^]
	    [mzlib:function : mzlib:function^])

    (mred:debug:printf 'invoke "mred:gui-utils@")

    (define get-colour-from-user 
      (lambda (prompt init)
	(if (wx:can-get-user-colour?)
	    (wx:get-colour-from-user prompt init)
	    (wx:message-box "not yet implemented"))))
    
    (define get-font-from-user
      (lambda (prompt init)
	(if (wx:can-get-user-font?)
	    (wx:get-font-from-user prompt init)
	    (wx:message-box "not yet implemented"))))

    (define get-text-from-user
      (opt-lambda (message [caption "Input text"]
			   [default-value ""]
			   [parent null]
			   [x -1]
			   [y -1]
			   [center? #t])
	(let* ([f (make-object mred:container:dialog-box% parent caption #t x y)]
	       [p (make-object mred:container:vertical-panel% f)])
	  (if center?
	      (make-object mred:container:message% p message)
	      (let ([mp (make-object mred:container:horizontal-panel% p)])
		(make-object mred:container:message% mp message)
		(make-object mred:container:horizontal-panel% mp)))
	  (let* ([c (make-object mred:canvas:one-line-canvas% p)]
		 [e (make-object mred:edit:return-edit%
				 (lambda ()
				   (send f show #f)))]
		 [bp (make-object mred:container:horizontal-panel% p)]
		 [_ (make-object mred:container:horizontal-panel% bp)]
		 [hit-ok? #t]
		 [cancel (make-object mred:container:button% bp
				      (lambda (b evt)
					(set! hit-ok? #f)
					(send f show #f))
				      "Cancel")]
		 [ok (make-object mred:container:button% bp
				  (lambda (b evt) (send f show #f))
				  "OK")])
	    (send* e
	      (insert default-value)
	      (set-position 0 (string-length default-value)))
	    (send* c 
	      ;(set-lines 3) 
	      (set-media e)
	      (set-focus)
	      (user-min-width 300))
	    (send ok user-min-width (send cancel user-min-width))
	    (send f set-size 20 20 -1 -1)
	    (when (= -1 x y)
	      (send f centre))
	    (send f show #t)
	    (and hit-ok?
		(send e get-text))))))

    (define message-box
      (let ([e% (class-asi mred:edit:media-edit%
		  (public
		    [auto-set-wrap #t]
		    [autowrap-bitmap null]))])
	(opt-lambda (s [title "Message Box"])
	  (let* ([f (make-object mred:container:dialog-box% '() title #t)]
		 [p (make-object mred:container:vertical-panel% f)]
		 [c (make-object mred:canvas:wrapping-canvas% p)]
		 [e (make-object mred:edit:media-edit%)]
		 [bottom-panel (make-object mred:container:horizontal-panel% p)]
		 [space (make-object mred:container:horizontal-panel%
				     bottom-panel)]
		 [button (make-object mred:container:button% bottom-panel
				      (lambda x (send f show #f))
				      "Ok")])
	    (send c set-media e)
	    (send e set-auto-set-wrap #t) ;; this should be supurflous
	    (send e set-autowrap-bitmap null) ;; this should also not be necessary
	    (send* e (insert s) (lock #t))
	    (send button set-focus)
	    (send bottom-panel stretchable-in-y #f)
	    (send f set-size -1 -1 400 200)
	    (send f center wx:const-both)
	    (send f show #t)
	    f))))

    (define cursor-delay
      (let ([x 0.25])
	(case-lambda
	 [() x]
	 [(v) (set! x v) x])))

    (define show-busy-cursor
      (lambda (thunk)
	(local-busy-cursor #f thunk)))

    (define delay-action
      (lambda (delay-time open close)
	(let ([semaphore (make-semaphore 1)]
	      [open? #f]
	      [skip-it? #f])
	  (thread 
	   (lambda ()
	     (sleep delay-time)
	     (semaphore-wait semaphore)
	     (unless skip-it?
	       (set! open? #t)
	       (open))
	     (semaphore-post semaphore)))
	  (lambda ()
	    (semaphore-wait semaphore)
	    (if open?
		(close)
		(set! skip-it? #t))
	    (semaphore-post semaphore)))))

    (define local-busy-cursor
      (let ([watch (make-object wx:cursor% wx:const-cursor-watch)])
	(opt-lambda (win thunk [delay (cursor-delay)])
	  (let* ([old-cursor #f]
		 [cursor-off
		  (delay-action
		   delay
		   (lambda ()
		     (if win
			 (set! old-cursor (send win set-cursor watch))
			 (wx:begin-busy-cursor)))
		   (lambda ()
		     (if win
			 (send win set-cursor old-cursor)
			 (wx:end-busy-cursor))))])
	    (dynamic-wind
	     void
	     thunk
	     cursor-off)))))

    (define get-choice
      (opt-lambda (message true-choice false-choice 
			   [title "Warning"][x -1][y -1])
	(let* ([result (void)]
	       [dialog%
		(class wx:dialog-box% ()
		  (inherit show new-line fit tab center)
		  (private
		    [on-true
		     (lambda args
		       (set! result #t)
		       (show #f))]
		    [on-false
		     (lambda rags
		       (set! result #f)
		       (show #f))])
		  (sequence
		    (super-init () title #t x y)
		    (let* ([messages
			    (let loop ([m message])
			      (let ([match (regexp-match (format "([^~n]*)~n(.*)")
							 m)])
				(if match
				    (cons (cadr match)
					  (loop (caddr match)))
				    (list m))))]
			   [msgs (map
				  (lambda (message)
				    (begin0
				      (make-object wx:message% this message)
				      (new-line)))
				  messages)])
		      
		      (send (make-object wx:button% this
					 on-true true-choice)
			    set-focus)
		      (tab 50)
		      (make-object wx:button% this on-false false-choice)
		      (fit)
		      
		      (if (and (< x 0) (< y 0))
			  (map (lambda (msg)
				 (send msg center wx:const-horizontal))
			       msgs)))
		    
		    (center wx:const-both)
		    
		    (show #t)))])
	  (make-object dialog%)
	  result)))
    
    (define unsaved-warning
      (opt-lambda (filename action [can-save-now? #f])
	(let* ([result (void)]
	       [dialog%
		(class wx:dialog-box% ()
		  (inherit show new-line fit tab center)
		  (private
		    [on-dont-save
		     (lambda args
		       (set! result 'continue)
		       (show #f))]
		    [on-save-now
		     (lambda rags
		       (set! result 'save)
		       (show #f))]
		    [on-cancel
		     (lambda args
		       (set! result 'cancel)
		       (show #f))])
		  (sequence
		    (super-init () "Warning" #t)
		    (let ([msg
			   (make-object wx:message% this
					(string-append "The file \""
						       filename
						       "\" is not saved."))])
		      (new-line)
		      (make-object wx:button% this
				   on-dont-save 
				   (string-append action " anyway"))
		      
		      (tab 50)
		      (let ([now (make-object wx:button% this
					      on-save-now "Save")]
			    [cancel (make-object wx:button% this
						 on-cancel "Cancel")])
			(if (not can-save-now?)
			    (begin (send cancel set-focus)
				   (send now show #f))
			    (send now set-focus)))
		      (fit)
		      (send msg center wx:const-horizontal))
		    
		    (center wx:const-both)
		    
		    (show #t)))])
	  (make-object dialog%)
	  result)))
    
    (define read-snips/chars-from-buffer
      (opt-lambda (edit [start 0] [end (send edit last-position)])
        (let ([pos start]
	      [box (box 0)])
	  (lambda ()
	    (let* ([snip (send edit find-snip pos
			       wx:const-snip-after-or-null box)]
		   [ans
		    (cond
		      [(<= end pos) eof]
		      [(null? snip) eof]
		      [(is-a? snip wx:text-snip%)
		       (let ([t (send snip get-text (- pos (unbox box)) 1)])
			 (unless (= (string-length t) 1)
			   (error 'read-snips/chars-from-buffer
				  "unexpected string, t: ~s; pos: ~a box: ~a"
				  t pos box))
			 (string-ref t 0))]
		      [else snip])])
	      (set! pos (add1 pos))
	      ans)))))

    (define open-input-buffer
      (lambda (buffer)
	(let ([pos 0])
	  (make-input-port
	   (lambda ()
	     (let ([c (send buffer get-character pos)])
	       (if (char=? c #\null)
		   eof
		   (begin
		     (set! pos (add1 pos))
		     c))))
	   (lambda ()
	     #t)
	   (lambda ()
	     (void))))))
    
    (define get-single-choice
      (opt-lambda (message caption choices 
			   [parent null]
			   [x -1] 
			   [y -1]
			   [centre #t]
			   [width 150]
			   [height 250])
	(let* ([frame (make-object mred:container:dialog-box% parent caption 
				   #t x y width height)]
	       [panel (make-object mred:container:vertical-panel% frame)]
	       [msg (make-object mred:container:message% panel message)]
	       [list-box (make-object mred:container:list-box% panel null null wx:const-single
				      -1 -1 -1 -1 choices)]
	       [button-panel (make-object mred:container:horizontal-panel% panel)]
	       [ok? #t]
	       [_ (make-object mred:container:horizontal-panel% button-panel)]
	       [ok (make-object mred:container:button% button-panel 
				(lambda (button evt)
				  (set! ok? #t)
				  (send frame show #f))
				"Ok")]
	       [cancel (make-object mred:container:button% button-panel 
				    (lambda (button evt)
				      (set! ok? #f)
				      (send frame show #f))
				    "Cancel")])
	  (when centre
	    (send frame center wx:const-both))
	  (send button-panel stretchable-in-y #f)
	  (send ok user-min-width (send cancel user-min-width))
	  (send frame show #t)
	  (if ok?
	      (send list-box get-string-selection)
	      null))))

    ; For use with wx:set-print-paper-name
    (define print-paper-names
      (list
       "A4 210 x 297 mm"
       "A3 297 x 420 mm"
       "Letter 8 1/2 x 11 in"
       "Legal 8 1/2 x 14 in")))
