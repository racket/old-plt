
(module async-channel mzscheme
  (require (lib "etc.ss")
	   (lib "contract.ss"))

  ;; This library implements buffered channels with
  ;; and optional buffer limit (so that puts block
  ;; if the buffer is full).

  ;; We make a fancy structure just so an async-channel
  ;; can be supplied directly to 'object-wait-multiple'.
  ;; The alternative is to use `define-struct' and supply
  ;; a `make-async-channel-get-waitable' procedure.
  (define-values (struct:ac make-ac async-channel? ac-ref ac-set!)
    (make-struct-type 'async-channel #f 5 0 #f 
		      (list (cons prop:waitable 
				  ;; This is the guard that is called when
				  ;;  we use an async-channel as a waitable
				  ;;  (to get).
				  (lambda (ac)
				    (async-channel-get-guard ac))))
		      (current-inspector) #f))
  (define ac-enqueue-ch (make-struct-field-accessor ac-ref 0))
  (define ac-dequeue-ch (make-struct-field-accessor ac-ref 1))
  (define ac-empty-ch (make-struct-field-accessor ac-ref 2))
  (define ac-full-ch (make-struct-field-accessor ac-ref 3))
  (define ac-thread (make-struct-field-accessor ac-ref 4))
    
  ;; Make ----------------------------------------

  (define make-async-channel
    (opt-lambda ([limit #f])
      (let* ([enqueue-ch (make-channel)]  ; for puts
	     [dequeue-ch (make-channel)]  ; for gets
	     [empty-ch (make-channel)]    ; for get polls
	     [full-ch (make-channel)]     ; for put polls
	     [queue-first (cons #f null)] ; queue head
	     [queue-last queue-first]     ; queue tail
	     ;; Waitables:
	     [tell-empty 
	      (make-channel-put-waitable empty-ch (make-semaphore))] ; see poll->ch
	     [tell-full 
	      (make-channel-put-waitable full-ch (make-semaphore))]  ; see poll->ch
	     [enqueue (make-wrapped-waitable
		       enqueue-ch
		       (lambda (v)
			 ;; We received a put; enqueue it:
			 (let ([p (cons #f null)])
			   (set-car! queue-last v)
			   (set-cdr! queue-last p)
			   (set! queue-last p))))]
	     [mk-dequeue
	      (lambda ()
		(make-wrapped-waitable
		 (make-channel-put-waitable dequeue-ch (car queue-first))
		 (lambda (ignored)
		   ;; A get succeeded; dequeue it:
		   (set! queue-first (cdr queue-first)))))]
	     [manager-thread
	      ;; This thread is the part that makes the channel asynchronous.
	      ;; It waits for a combination of gets and puts as appropriate.
	      ;; Note that we start it with `thread/suspend-kill', and we
	      ;; resume the manager thread with the current thread everytime
	      ;; we want to talk to the manager thread, which effectively
	      ;; means that the manager thread is not bound by a custodian
	      ;; that is weaker than any of its user's custodians (and thus,
	      ;; from the user's perspective, is not bound by any custodian
	      ;; at all).
	      (thread/suspend-to-kill
	       (lambda ()
		 (let loop ()
		   (cond
		    [(= 1 (length queue-first))
		     ;; The queue is currently empty:
		     (object-wait-multiple #f enqueue tell-empty)]
		    [(or (not limit) ((sub1 (length queue-first)) . < . limit))
		     (object-wait-multiple #f enqueue (mk-dequeue))]
		    [else
		     (object-wait-multiple #f (mk-dequeue) tell-full)])
		   (loop))))])
	(make-ac enqueue-ch dequeue-ch empty-ch full-ch manager-thread))))

  ;; Get ----------------------------------------
  
  (define (async-channel-get-guard ac)
    ;; Make sure queue manager is running:
    (thread-resume (ac-thread ac) (current-thread))
    ;; If it the channel is being polled, it's not
    ;;  good enough to poll the dequeue channel, because
    ;;  the server thread may be looping. In that case,
    ;;  block on the dequeue channel and the empty
    ;;  channel, and create a new waitable to report
    ;;  the result.
    (make-poll-guard-waitable
     (lambda (poll?)
       (if poll?
	   (poll->ch (ac-dequeue-ch ac) (ac-empty-ch ac))
	   (ac-dequeue-ch ac)))))

  (define (async-channel-get ac)
    (object-wait-multiple #f ac))

  (define (async-channel-try-get ac)
    (object-wait-multiple 0 ac))

  ;; Put ----------------------------------------

  (define (make-async-channel-put-waitable ac v)
    (letrec ([p (make-wrapped-waitable
		 (make-guard-waitable
		  (lambda ()
		    ;; Make sure queue manager is running:
		    (thread-resume (ac-thread ac) (current-thread))
		    (let ([p (make-channel-put-waitable (ac-enqueue-ch ac) v)])
		      ;; Poll handling, as in `async-channel-get-guard':
		      (make-poll-guard-waitable
		       (lambda (poll?)
			 (if poll?
			     (poll->ch p (ac-full-ch ac))
			     p))))))
		 (lambda (ignored) p))])
      p))
  
  (define (async-channel-put ac v)
    (thread-resume (ac-thread ac) (current-thread))
    (object-wait-multiple #f (make-channel-put-waitable (ac-enqueue-ch ac) v))
    (void))

  ;; Poll helper ----------------------------------------

  (define (poll->ch normal not-ready)
    (object-wait-multiple #f
			  ;; If a value becomes available,
			  ;;  create a waitable that returns
			  ;;  the value:
			  (make-wrapped-waitable
			   normal
			   (lambda (v)
			     ;; Return a waitable for a successful poll:
			     (make-wrapped-waitable
			      (make-semaphore 1)
			      (lambda (ignored) v))))
			  ;; If not-ready becomes available,
			  ;;  the result is supposed to be
			  ;;  a never-ready waitable:
			  not-ready))

  ;; Provides ----------------------------------------

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
