
  (unit/sig mred:gui-utils^
    (import [mred:frame : mred:frame^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:edit : mred:edit^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:trigger : mzlib:trigger^])

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

    (define message-box
      (let ([e% (class-asi mred:edit:edit%
		  (public
		    [auto-set-wrap #t]
		    [autowrap-bitmap null]))])
	(opt-lambda (s [title "Message Box"])
	  (let* ([f (make-object mred:container:dialog-box% '() title #t)]
		 [p (make-object mred:container:vertical-panel% f)]
		 [c (make-object mred:canvas:wrapping-canvas% p)]
		 [e (make-object mred:edit:edit%)]
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
	    (send* f (set-size -1 -1 400 200) (show #t))
	    f))))

    (define cursor-delay
      (let ([x 0.25])
	(case-lambda
	 [() x]
	 [(v) (set! x v)])))

    (define show-busy-cursor
      (lambda (thunk)
	(local-busy-cursor #f thunk)))

    (define delay-action
      (lambda (delay-time open close)
	(let ([trigger (mzlib:trigger:make-trigger)]
	      [open-done-trigger (mzlib:trigger:make-trigger)])
	  (thread 
	   (lambda ()
	     (sleep delay-time)
	     (when (mzlib:trigger:trigger-test-and-hit? trigger)
	       (open)
	       (mzlib:trigger:trigger-hit open-done-trigger))))
	  (lambda ()
	    (when (not (mzlib:trigger:trigger-test-and-hit? trigger))
	      (mzlib:trigger:trigger-block open-done-trigger #t)
	      (close))))))

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
    
    ; For use with wx:set-print-paper-name
    (define print-paper-names
      (list
       "A4 210 x 297 mm"
       "A3 297 x 420 mm"
       "Letter 8 1/2 x 11 in"
       "Legal 8 1/2 x 14 in")))
