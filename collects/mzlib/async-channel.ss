
(module async-channel mzscheme
  (require (lib "etc.ss")
	   (lib "contract.ss"))

  (define-values (struct:ac make-ac async-channel? ac-ref ac-set!)
    (make-struct-type 'async-channel #f 3 0 #f 
		      (list (cons prop:waitable 
				  ;; This is the guard that is called when
				  ;;  we use an async-channel as a waitable
				  ;;  (to get).
				  (lambda (ac)
				    ;; Make sure queue manager is running:
				    (thread-resume (ac-thread ac) (current-thread))
				    (ac-dequeue-ch ac))))
		      (current-inspector) #f '(0)))

  (define ac-enqueue-ch (make-struct-field-accessor ac-ref 0))
  (define ac-dequeue-ch (make-struct-field-accessor ac-ref 1))
  (define ac-thread (make-struct-field-accessor ac-ref 2))
    
  (define make-async-channel
    (opt-lambda ([limit #f])
      (let* ([enqueue-ch (make-channel)]
	     [dequeue-ch (make-channel)]
	     [queue-first (cons #f null)]
	     [queue-last queue-first]
	     [manager-thread
	      (thread/suspend-to-kill
	       (lambda ()
		 (let loop ()
		   (let ([mk-dequeue
			  (lambda ()
			    (make-wrapped-waitable
			     (make-channel-put-waitable dequeue-ch (car queue-first))
			     (lambda (ignored)
			       (set! queue-first (cdr queue-first))
			       (loop))))]
			 [mk-enqueue
			  (lambda ()
			    (make-wrapped-waitable
			     enqueue-ch
			     (lambda (v)
			       (let ([p (cons v null)])
				 (set-cdr! queue-last p)
				 (set! queue-last p)
				 (loop)))))])
		     (cond
		      [(= 1 (length queue-first))
		       (object-wait-multiple #f (mk-enqueue))]
		      [(or (not limit) ((sub1 (length queue-first)) . < . limit))
		       (object-wait-multiple #f (mk-enqueue) (mk-dequeue))]
		      [else
		       (object-wait-multiple #f (mk-dequeue))])))))])
	(make-ac enqueue-ch dequeue-ch manager-thread))))

  (define (async-channel-get ac)
    (object-wait-multiple #f ac))

  (define (async-channel-try-get ac)
    (object-wait-multiple 0 ac))

  (define (make-async-channel-put-waitable ac v)
    (make-guard-waitable
     (lambda ()
       ;; Make sure queue manager is running:
       (thread-resume (ac-thread ac) (current-thread))
       (make-channel-put-waitable (ac-enqueue-ch ac) v))))
  
  (define (async-channel-put ac v)
    (object-wait-multiple #f (make-async-channel-put-waitable ac v))
    (void))

  (provide async-channel?)
  (provide/contract (make-async-channel (case->
					 (-> async-channel?)
					 ((union false? (lambda (x)
							   (and (integer? x)
								(exact? x)
								(positive? x))))
					  . -> . async-channel?)))
		    (async-channel-get (async-channel? . -> . any?))
		    (async-channel-try-get (async-channel? . -> . any?))
		    (async-channel-put (async-channel? any? . -> . any?))
		    (make-async-channel-put-waitable (async-channel? any? . -> . object-waitable?))))
