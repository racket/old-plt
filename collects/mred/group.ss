
  (unit/sig mred:group^
    (import [mred:constants : mred:constants^]
	    [mred:preferences : mred:preferences^]
	    [mred:editor-frame : mred:editor-frame^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:exit : mred:exit^]
	    [mred:autosave : mred:autosave^]
	    [mred:handler : mred:handler^]
	    [mred:application : mred:application^]
	    [mzlib:function : mzlib:function^])
	    
    (mred:debug:printf 'invoke "mred:group@")

    (define buffer-group%
      (let ([untitled-number 0])
	(let-struct buffer (name type edit modified-ok)
	  (class '() ()
	    (private
	      [buffers '()])
	    (public
	      [autosave? (mred:preferences:get-preference
			  'mred:autosaving-on?)]
	      [for-each-buffer
	       (lambda (f)
		 (for-each
		  (lambda (b)
		    (f (buffer-type b)
		       (buffer-name b)
		       (buffer-edit b)))
		  buffers))]
	      [find-buffer
	       (opt-lambda (type pred [otherwise (lambda () #f)])
		 (catch exit
		   (for-each
		    (lambda (b)
		      (if (and (eq? (buffer-type b) type)
			       (pred (buffer-name b)
				     (buffer-edit b)))
			  (exit (buffer-edit b))))
		    buffers)
		   (otherwise)))]
	      [find-buffer-by-name
	       (lambda (type name)
		 (find-buffer type
			      (lambda (bname edit)
				(if bname
				    (string=? bname name)
				    (equal? name 
					    (send edit get-filename))))))]
	      [add-buffer
	       (lambda (type name edit)
		 (set! buffers (cons (make-buffer name type edit #f) 
				     buffers)))]
	      [kill-buffer
	       (lambda (edit)
		 (set! buffers 
		       (mzlib:function:remove edit buffers
					      (lambda (edit b)
						(eq? edit (buffer-edit b))))))]
	      
	      [reset-buffer-state
	       (lambda (edit)
		 (for-each
		  (lambda (buffer)
		    (if (eq? (buffer-edit buffer) edit)
			(set-buffer-modified-ok! buffer #f)))
		  buffers))]
	      
	      [check-buffers-for-quit
	       (lambda ()
		 (check-buffers "Quit"))]
	      [check-buffers
	       (opt-lambda ([reason "Close"][pred #f])
		 (let loop ([buffers buffers])
		   (unless (null? buffers)
		     (let* ([buffer (car buffers)]
			    [type (buffer-type buffer)]
			    [modified-ok (buffer-modified-ok buffer)]
			    [edit (buffer-edit buffer)])
		       (if (and (eq? type 'file)
				(not modified-ok)
				(send edit modified?)
				(or (not pred)
				    (pred edit)))
			   (let* ([name (send edit get-filename)]
				  [name (if (null? name)
					    "Untitled"
					    name)])
			     (let ([action
				    (mred:gui-utils:unsaved-warning name 
								    reason
								    #t)])
			       (case action
				 [(cancel) #f]
				 [(continue)
				  (set-buffer-modified-ok! buffer #t)
				  (if autosave?
				      (send edit remove-autosave))
				  (check-buffers reason pred)]
				 [(save)
				  (and (send edit save-file)
				       (check-buffers reason pred))])))
			   (loop (cdr buffers)))))))]
	      [make-untitled-name
	       (lambda ()
		 (set! untitled-number (add1 untitled-number))
		 (string-append "Untitled " 
				(number->string untitled-number)))]
	      
	      [clear
	       (lambda ()
		 (set! buffers '())
		 #t)]
	      
	      [pick
	       (lambda (canvas)
		 (let ([which
			(wx:get-single-choice-index
			 "Select a buffer"
			 "Buffers"
			 (map (lambda (buffer)
				(let ([name (buffer-name buffer)])
				  (if name
				      name
				      (send (buffer-edit buffer) 
					    get-filename))))
			      buffers)
			 () -1 -1 #t 400 200)])
		   (if (>= which 0)
		       (send canvas set-media
			     (buffer-edit (list-ref buffers which))))))]
	      
	      [do-autosave
	       (lambda ()
		 (for-each
		  (lambda (buffer)
		    (unless (buffer-modified-ok buffer)
		      (send (buffer-edit buffer) do-autosave)))
		  buffers))])
	    (sequence
	      (mred:exit:insert-exit-callback check-buffers-for-quit)
	      (if autosave?
		  (mred:autosave:register-autosave this)))))))

    (define frame-group%
      (let ([b-group% buffer-group%])
	(let-struct frame (frame id)
	  (class '() ()
	    (private
	      [active-frame #f]
	      [frame-counter 0]
	      [frames null]
	      [todo-to-new-frames void])
	    
	    (public
	      [empty-callback (lambda () #f)]
	      [set-empty-callback (lambda (x) (set! empty-callback x))]
	      [get-frames (lambda () (map frame-frame frames))]
	      [buffer-group% b-group%]
	      [frame% mred:editor-frame:editor-frame%]
	      [get-frame% (lambda () frame%)]
	      
	      [get-buffer-group% (lambda () buffer-group%)]
	      [ask-before-closing-last? #t]
	      [frame-title-prefix mred:application:app-name]
	      [make-full-frame-prefix
	       (lambda (id)
		 (string-append 
		  frame-title-prefix
		  (if (= 1 id)
		      ""
		      (string-append
		       " #"
		       (number->string id)))))]

	      [for-each-frame
	       (lambda (f)
		 (for-each (lambda (x) (f (frame-frame x))) frames)
		 (set! todo-to-new-frames
		       (let ([old todo-to-new-frames])
			 (lambda (frame) (old frame) (f frame)))))]

	      [set-frame-title-prefix
	       (lambda (t)
		 (if (string? t)
		     (begin
		       (set! frame-title-prefix t)
		       (for-each
			(lambda (f)
			  (send (frame-frame f) set-title-prefix
				(make-full-frame-prefix (frame-id f))))
			frames))))]
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
		 '(send f set-title-prefix
		       (make-full-frame-prefix frame-counter))
		 (set! frames 
		       (append frames 
			       (list 
				(make-frame f frame-counter))))
		 (todo-to-new-frames f))]
	      
              [remove-frame
               (opt-lambda (f [reason "Close"])
                 (if (eq? f active-frame)
                     (set! active-frame #f))
                 (catch exit
		   (let ([new-frames
			  (mzlib:function:remove
			   f frames
			   (lambda (f fr)
			     (if (eq? f (frame-frame fr))
				 (begin
				   (if (and (null? (cdr frames))
					    ask-before-closing-last?
					    (not (send buffers 
						       check-buffers
						       reason)))
				       (exit #f))
				   #t)
				 #f)))])
		     (if (or (not (null? new-frames))
			     (empty-callback))
			 (begin (set! frames new-frames)
				#t)
			 #f))))]
	      [clear
	       (lambda ()
		 (when (empty-callback)
		   (if (send buffers clear)
		       (begin 
			 (set! frames ())
			 #t)
		       #f)))]
	      [close-all
	       (lambda ()
		 (catch escape
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
					       (make-object frame% fn #t group)))
		     (make-object (get-frame%) filename #t this)))]
	      [new-frame-for-buffer
	       (lambda (buffer)
		 (new-frame buffer))]
	      [open-file
	       (lambda (name)
		 (let* ([b (send buffers find-buffer-by-name 'file name)]
			[f (if b
			       (send b get-frame)
			       #f)])
		   (if f
		       (begin
			 (send f show #t)
			 (send f iconize #f))
		       (new-frame name))))])
	    (public
	      [buffers (make-object (get-buffer-group%))])))))
    
    (define current-frames (make-parameter
			    #f
			    (lambda (x)
			      (if (or (not x)
				      (is-a? x frame-group%))
				  x
				  (raise 'illegal-current-frames)))))

    (define (keep-frames)
      (unless (current-frames)
	(current-frames (make-object frame-group%)))))
