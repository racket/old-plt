;; mysterx.ss

(require-library "function.ss")
(require-library "string.ss")

(load-extension "mysterx.dll")

; list of mx-document, thread descriptor pairs
(define *handler-threads* '())

; hash table of hash tables keyed by mx-documents
(define *handler-table* (make-hash-table))

; semaphores
(define mx-thread-sem (make-semaphore 1))   ; protects *handler-threads*
(define mx-handler-sem (make-semaphore 1))  ; protects *handler-table* and its contained hash tables

; string x string -> symbol
(define (make-event-key tag id)
  (let ([new-tag (string-copy tag)]
	[new-id (string-copy id)])
    (string-uppercase! new-tag)
    (string-uppercase! new-id)
    (string->symbol
     (string-append new-tag "@" new-id))))

(define (register-event-handler doc tag id fn)
  (unless (document? doc)
	  (raise 'bad-handler-args))
  (semaphore-wait mx-handler-sem)
  (let* ([tag-string (symbol->string tag)]
	 [id-string (symbol->string id)])
    (let ([key (make-event-key tag-string id-string)]
          ; create inner hash table if none exists
	  [doc-table (hash-table-get *handler-table* doc
				     (lambda () 
				       (hash-table-put! *handler-table* doc 
							(make-hash-table))
				       (hash-table-get *handler-table* doc)))])
      (hash-table-remove! doc-table key)
      (hash-table-put! doc-table key fn)))
  (semaphore-post mx-handler-sem))

(define (unregister-event-handler doc tag id)
  (unless (document? doc)
	  (raise 'bad-handler-args))
  (semaphore-wait mx-handler-sem)
  (let* ([tag-string (symbol->string tag)]
	 [id-string (symbol->string id)])
    (let ([key (make-event-key tag-string id-string)]
	  [doc-table (hash-table-get *handler-table* doc void)])
      (unless (void? doc-table)
	      (hash-table-remove! doc-table key))))
  (semaphore-post mx-handler-sem))

; busy-wait loop -- until Mz threads allow blocking
(define (block-until-event doc)
  (let loop () 
    (unless (event-available? doc)
	    (sleep 0.1)
	    (loop))))

(define (handle-events doc) 
  (semaphore-wait mx-thread-sem)
  (let ([thread-docs (map car *handler-threads*)])
    (unless (memq doc thread-docs)
	    (semaphore-wait mx-handler-sem)
	    (let* ([doc-table (hash-table-get *handler-table* doc)]
		   [handler-thunk
		    (lambda ()
		      (let loop ()
			(block-until-event doc)
			   (let* ([event (get-event doc)]
				  [tag (event-tag event)]
				  [id (event-id event)]
				  [key (make-event-key tag id)]
				  [handler (hash-table-get doc-table key void)])
			     (unless (void? handler)
				     (handler event))
			     (loop))))])
	      (set! *handler-threads*
		    (cons 
		     (cons doc (thread handler-thunk))
		     *handler-threads*)))
	    (semaphore-post mx-handler-sem)))
  (semaphore-post mx-thread-sem))

(define (stop-handling-events doc) 
  (semaphore-wait mx-thread-sem)
  (let ([doc-pair (assq doc *handler-threads*)])
    (when doc-pair
	  (thread-kill (cdr doc-pair))
	  (set! *handler-threads* (remove doc-pair *handler-threads*))))
  (semaphore-post mx-thread-sem))

(define (insert-control! doc control)
	(insert-html doc (object->html control))
	(car (document-objects doc)))

(define (append-control! doc control)
	(insert-html doc (object->html control))
	(car (last-pair (document-objects doc))))

			      
			 
	    


       
       
    
	