;; mysterx.ss

(require-library "function.ss")
(require-library "string.ss")

(load-extension "mysterx.dll")

(define mx-event%
  (class null (dhtml-event)

	 (private
	  [event dhtml-event])

	 (public

	  ; predicates

	  [keypress? (lambda () (event-keypress? event))]
	  [keydown? (lambda () (event-keydown? event))]
	  [keyup? (lambda () (event-keyup? event))] 
	  [mousedown? (lambda () (event-mousedown? event))] 
	  [mousemove? (lambda () (event-mousemove? event))] 
	  [mouseover? (lambda () (event-mouseover? event))] 
	  [mouseout? (lambda () (event-mouseout? event))] 
	  [mouseup? (lambda () (event-mouseup? event))] 
	  [click? (lambda () (event-click? event))] 
	  [dblclick? (lambda () (event-dblclick? event))] 
	  [error? (lambda () (event-error? event))]
	  
	  ; attributes

	  [tag (lambda () (event-tag event))]
	  [id (lambda () (event-id event))]
	  [from-tag (lambda () (event-from-tag event))]
	  [from-id (lambda () (event-id event))]
	  [to-tag (lambda () (event-to-tag event))]
	  [to-id (lambda () (event-to-id event))]
	  [x (lambda () (event-x event))]
	  [y (lambda () (event-y event))])))

(define mx-document%
  (class null 

	 ((label "MysterX")
	  (width 'default)
	  (height 'default)
	  (x 'default)
	  (y 'default)
	  (style-options null))

	 (private
	  [doc (make-document label width height x y style-options)]
	  [thread-sem (make-semaphore 1)]   ; protects *handler-threads*
	  [handler-sem (make-semaphore 1)]  ; protects *handler-table* and its contained hash tables
	  [handler-table (make-hash-table)]
	  [handler-thread #f]
	  [block-until-event ; busy-wait loop -- until Mz threads allow blocking
	   (lambda ()
	     (let loop () 
	       (unless (event-available? doc)
		       (sleep 0.1)
		       (loop))))]
	  [make-event-key 
	   (lambda (tag id) ; string x string -> symbol
	     (let ([new-tag (string-copy tag)]
		   [new-id (string-copy id)])
	       (string-uppercase! new-tag)
		  (string-uppercase! new-id)
		  (string->symbol
		   (string-append new-tag "@" new-id))))])

	 (public
	  [show 
	   (lambda (b) 
	     (document-show doc b))]
	  [objects
	   (lambda () 
	     (document-objects doc))]
	  [insert-html 
	   (lambda (html-string)
	     (document-insert-html doc html-string))]
	  [append-html 
	   (lambda (html-string)
	     (document-append-html doc html-string))]
	  [replace-html 
	   (lambda (html-string)
	     (document-replace-html doc html-string))]
	  [register-event-handler
	   (lambda (tag id fn)
	     (semaphore-wait handler-sem)
	     (let* ([tag-string (symbol->string tag)]
		    [id-string (symbol->string id)])
	       (let ([key (make-event-key tag-string id-string)])
		 (hash-table-remove! handler-table key)
		 (hash-table-put! handler-table key fn)))
	     (semaphore-post handler-sem))]
	  [unregister-event-handler
	   (lambda (tag id)
	     (semaphore-wait handler-sem)
	     (let* ([tag-string (symbol->string tag)]
		    [id-string (symbol->string id)])
	       (let ([key (make-event-key tag-string id-string)])
		 (hash-table-remove! handler-table key)))
	     (semaphore-post handler-sem))]
	   [append-object 
	    (lambda (object)
	      (append-html (coclass->html object))
	      (car (document-objects doc)))]
	   [insert-object 
	    (lambda (object)
	      (insert-html (coclass->html object))
	      (car (document-objects doc)))]
	   [handle-events 
	    (lambda ()
	      (semaphore-wait thread-sem)
	      ; no-op if existing handler-thread
	      (unless handler-thread
		      (semaphore-wait handler-sem)
		      (let* ([handler-thunk
			      (lambda ()
				(let loop ()
				  (block-until-event)
				     (let* ([event (make-object mx-event% 
								(get-event doc))]
					    [tag (send event tag)]
					    [id (send event id)]
					    [key (make-event-key tag id)]
					    [handler (hash-table-get handler-table key void)])
				       (unless (void? handler)
					       (handler event))
				       (loop))))])
			(set! handler-thread (thread handler-thunk)))
		      (semaphore-post handler-sem))
	      (semaphore-post thread-sem))]
	  [stop-handling-events 
	   (lambda ()
	     (semaphore-wait thread-sem)
	     (when handler-thread
		   (kill-thread handler-thread))
	     (set! handler-thread #f)
	     (semaphore-post thread-sem))])))

	  
	 
		


       
       
    
	

