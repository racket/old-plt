(unit/sig mred:group^
  (import [wx : wx^]
	  [mred:constants : mred:constants^]
	  [mred:preferences : mred:preferences^]
	  [mred:editor-frame : mred:editor-frame^]
	  [mred:gui-utils : mred:gui-utils^]
	  [mred:exit : mred:exit^]
	  [mred:autosave : mred:autosave^]
	  [mred:handler : mred:handler^]
	  [mzlib:function : mzlib:function^]
	  [mzlib:file : mzlib:file^])
  
  (mred:debug:printf 'invoke "mred:group@")
  
  (define windows-menu-name "Windows")
  
  (define frame-group%
    (let-struct frame (frame id)
      (class null ()
	(private
	  [active-frame #f]
	  [frame-counter 0]
	  [frames null]
	  [todo-to-new-frames void]
	  [empty-close-down (lambda () (void))]
	  [empty-test (lambda () #t)]
	  
	  [windows-menus null])
	
	(private
	  [insert-windows-menu
	   (lambda (frame)
	     (let ([mb (send frame get-menu-bar)])
	       (unless (send mb get-menu-named window-menu-name)
		 (let ([menu (send frame make-menu)])
		   (set! windows-menus (cons menu windows-menus))
		   (send mb append menu windows-menu-name)))))]
	  [remove-windows-menu
	   (lambda (frame)
	     (let* ([mb (send frame get-menu-bar)]
		    [menu (send mb get-menu-named windows-menu-name)])
	       (set! windows-menus
		     (mzlib:function:remq menu windows-menus))
	       (send mb delete menu -1)))]
	  
	  [update-windows-menus
	   (lambda ()
	     (printf "frames: ~e ~e~n" (length frames) frames)
	     (let* ([windows (length windows-menus)]
		    [get-name (lambda (frame) (send (frame-frame frame) get-title))]
		    [sorted-frames
		     (mzlib:function:quicksort
		      frames
		      (lambda (f1 f2)
			(string-ci<=? (get-name f1)
				      (get-name f2))))])
	       (for-each
		(lambda (menu)
		  (let loop ([n (+ windows 1)])
		    (unless (zero? n)
		      (send menu delete-by-position 0)
		      (loop (sub1 n))))
		  (for-each
		   (lambda (frame)
		     (let ([frame (frame-frame frame)])
		       (printf "menu: ~a frame: ~a~n" menu (send frame get-title))
		       (send menu append-item
			     (send frame get-title)
			     (lambda ()
			       (send frame show #t)))))
		   sorted-frames))
		windows-menus)))])
	       
	
	(public
	  [set-empty-callbacks 
	   (lambda (test close-down) 
	     (set! empty-test test)
	     (set! empty-close-down close-down))]
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
	       (set! frames new-frames)
	       (insert-windows-menu f)
	       (update-windows-menus))
	     (todo-to-new-frames f))]
	  
	  [can-remove-frame?
	   (opt-lambda (f)
	     (let ([new-frames 
		    (mzlib:function:remove
		     f frames
		     (lambda (f fr) (eq? f (frame-frame fr))))])
	       (if (null? new-frames)
		   (empty-test)
		   #t)))]
	  [remove-frame
	   (opt-lambda (f)
	     (when (eq? f active-frame)
	       (set! active-frame #f))
	     (let ([new-frames
		    (mzlib:function:remove
		     f frames
		     (lambda (f fr) (eq? f (frame-frame fr))))])
	       (set! frames new-frames)
	       (remove-windows-menu f)
	       (update-windows-menus)
	       (when (null? frames)
		 (empty-close-down))))]
	  [clear
	   (lambda ()
	     (and (empty-test)
		  (begin (set! frames null)
			 (empty-close-down)
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
  
  (send the-frame-group set-empty-callbacks
	(lambda () 
	  (at-most-one #t
		       (lambda ()
			 (mred:exit:run-exit-callbacks))))
	(lambda () 
	  (at-most-one (void) 
		       (lambda () (mred:exit:exit #t)))))
  
  (mred:exit:insert-exit-callback
   (lambda ()
     (at-most-one
      #t
      (lambda ()
	(send the-frame-group close-all))))))
