
  (unit/sig mred:group^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:preferences : mred:preferences^]
	    [mred:editor-frame : mred:editor-frame^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:exit : mred:exit^]
	    [mred:autosave : mred:autosave^]
	    [mred:handler : mred:handler^]
	    [mred:application : mred:application^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:group@")

    (define frame-group%
      (let-struct frame (frame id)
	(class null ()
	  (private
	    [active-frame #f]
	    [frame-counter 0]
	    [frames null]
	    [todo-to-new-frames void])
	  
	  (public
	    [empty-callback (lambda () #f)]
	    [set-empty-callback (lambda (x) (set! empty-callback x))]
	    [get-frames (lambda () (map frame-frame frames))]
	    [frame% mred:editor-frame:editor-frame%]
	    [get-frame% (lambda () frame%)]
	    
	    [for-each-frame
	     (lambda (f)
	       (for-each (lambda (x) (f (frame-frame x))) frames)
	       (set! todo-to-new-frames
		     (let ([old todo-to-new-frames])
		       (lambda (frame) (old frame) (f frame)))))]
	    [get-active-frame
	     (lambda ()
	       (cond
		 [active-frame active-frame]
		 [(null? frames) #f]
		 [else (frame-frame (car frames))]))]
	    [set-active-frame
	     (lambda (f)
	       (set! active-frame f))]
	    [insert-frame
	     (lambda (f)
	       (set! frame-counter (add1 frame-counter))
	       (let ([new-frames (cons (make-frame f frame-counter)
				       frames)])
		 (set! frames new-frames))
	       (todo-to-new-frames f))]

	    [remove-frame
	     (opt-lambda (f [reason "Close"])
	       (when (eq? f active-frame)
		 (set! active-frame #f))
	       (let ([new-frames
		      (mzlib:function:remove
		       f frames
		       (lambda (f fr) (eq? f (frame-frame fr))))])
		 (let ([allow-changes
			(lambda ()
			  (set! frames new-frames)
			  #t)])
		   (if (null? new-frames)
		       (if (empty-callback)
			   (allow-changes)
			   #f)
		       (allow-changes)))))]
	    [clear
	     (lambda ()
	       (and (empty-callback)
		    (begin (set! frames null)
			   #t)))]
	    [close-all
              (lambda ()
                (let/ec escape
		  (for-each (lambda (f)
			      (let ([frame (frame-frame f)])
				(if (send frame on-close)
                                    (send frame show #f)
                                    (escape #f))))
			    frames)
		  #t))]
	    [new-frame
	     (lambda (filename)
	       (if (string? filename)
		   (mred:handler:edit-file filename this #f
					   (lambda (fn group)
					     (make-object (get-frame%)
					       fn #t group)))
		   (make-object (get-frame%) filename #t this)))]
	    [locate-file
	     (lambda (name)
	       (let* ([normalized
		       ;; allow for the possiblity of filenames that are urls
		       (with-handlers ([(lambda (x) #t)
					(lambda (x) name)])
			 (mzlib:file:normalize-path name))]
		      [test-frame
		       (lambda (frame)
			 (and (ivar-in-class? 'get-edit (object-class frame))
			      (let* ([edit (send frame get-edit)]
				     [filename (send edit get-filename)])
				(and (ivar edit editing-this-file?)
				     (string? filename)
				     (string=? normalized
					       (with-handlers ([(lambda (x) #t)
								(lambda (x) filename)])
						 (mzlib:file:normalize-path 
						  filename)))))))])
		 (let loop ([frames frames])
		   (cond
		     [(null? frames) #f]
		     [else
		      (let* ([frame (frame-frame (car frames))])
			(if (test-frame frame)
			    frame
			    (loop (cdr frames))))]))))]))))

    (define the-frame-group (make-object frame-group%))
    

    (define at-most-one-maker
      (lambda ()
	(let ([s (make-semaphore 1)]
	      [test #f])
	  (lambda (return thunk)
	    (semaphore-wait s)
	    (if test
		(begin (semaphore-post s)
		       return)
		(begin
		  (set! test #t)
		  (semaphore-post s)
		  (begin0 (thunk)
			  (semaphore-wait s)
			  (set! test #f)
			  (semaphore-post s))))))))

    (define at-most-one (at-most-one-maker))

    (send the-frame-group set-empty-callback
	  (lambda ()
	    (at-most-one
	     #t
	     (lambda ()
	       (mred:exit:exit)
	       #f))))

    (mred:exit:insert-exit-callback
     (lambda ()
       (at-most-one
	#t
	(lambda ()
	  (send the-frame-group close-all))))))
