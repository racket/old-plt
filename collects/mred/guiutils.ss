(define mred:gui-utils@
  (unit/sig mred:gui-utils^
    (import ([unit mred:debug : mred:debug^]
	     [unit mzlib:function : mzlib:function^]
	     [unit mzlib:trigger : mzlib:trigger^]))

    (mred:debug:printf 'invoke "mred:gui-utils@")

    (define cursor-delay
      (mzlib:function:make-parameter 0.25))

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
	(lambda (win thunk)		
	  (let* ([old-cursor #f]
		 [cursor-off
		  (delay-action
		   (cursor-delay)
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
			   
			   (make-object wx:button% this
					on-true true-choice)
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
						   on-save-now "Save")])
			     (if (not can-save-now?)
				 (send now show #f)))
			   
			   (make-object wx:button% this
					on-cancel "Cancel")
			   (fit)
			   
			   (send msg center wx:const-horizontal))
			 
			 (center wx:const-both)
			 
			 (show #t)))])
	  (make-object dialog%)
	  result)))
    
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
       "Legal 8 1/2 x 14 in"))
    ))
