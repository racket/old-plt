(module session mzscheme
  (require "session-queue.ss")
  
  (provide session->resource
           session?
           make-session-collection
           renew-session!
           kill-session!)
  
  ;; ********************************************************************************
  ;; General philosophy of sessions:
  ;;
  ;; A session manages a particular class of resource. e.g. connections, servlet-instances
  ;; Sessions may expire.
  ;;   e.g.: (1) a timer may time out and cause the session to expire.
  ;;         (2) in a queued model, a session may eventually be pushed out of a queue and expire.
  ;; When a session expires the resource managed by the session is destroyed.
  ;; At any time prior to expiration, a session may be renewed, given a time in seconds.
  ;;  The session is guaranteed to last at least as long as the provided time.
  ;; All session which manage a particular class belong to a collection.
  ;; Sessions in a collection are created via a curried function: make-create-session which returns
  ;;  a session creator for the collection.
  ;; A session may be killed befor its expiration time.
    
  ;; ********************************************************************************
  ;; DATA DEFINITION
  ;; A session is a structure
  ;; (make-session number number ( -> ))
  (define-struct session (start-time time-to-live payload shutdown! queue))
   
  (define session->resource session-payload)
  
  ;; ********************************************************************************
  ;; session creation
  
  ;; can-create-session?: session-queue -> boolean
  ;; is there enough memory to create a session assuming last GC unknown
  (define (can-create-session? a-session-queue)
    (cond
      [(get-sleep-time a-session-queue)
       => (lambda (tau)
            (or (<= tau 0)
                (enough-memory? a-session-queue)
                (sleep-cycle tau a-session-queue)))]
      [else ;; queue is empty
       (or (enough-memory? a-session-queue)
           (begin (collect-garbage)
                  (enough-memory? a-session-queue)))]))
  
  ;; sleep-cycle: number session-queue -> boolean
  (define (sleep-cycle tau a-session-queue)
    (collect-garbage) ;; gotta sleep anyway
    (or (enough-memory? a-session-queue)
        (begin
          (printf "about to sleep ~a~n" tau)
          (sleep tau)
          (printf "   done sleeping~n")
          (can-create-session? a-session-queue))))
  
  ;; enough-memory?: -> boolean
  (define (enough-memory? a-session-queue)
    (< (current-memory-use) (session-queue-memory-threshold a-session-queue)))
                  
;  (define (can-create-session? a-session-queue)
;    (or (< (current-memory-use) (session-queue-memory-threshold a-session-queue))
;        (can-create-session/gc? a-session-queue)))
  
  ;; can-create-session/gc?: session-queue -> boolean
  ;; try a gc first and then see if there is enough memory, 
  ;; if there is not enough memory, then wait for some sessions to become idle,
  ;; kill the idle sessions and try again.
  (define (can-create-session/gc? a-session-queue)
    (collect-garbage)
    (or (< (current-memory-use) (session-queue-memory-threshold a-session-queue))
        (and (wait-for-idle-session a-session-queue)
             (can-create-session/gc? a-session-queue))))
  
  ;; kill-old-session?: session-queue -> boolean
  ;; look for an old session and kill it or return #f
  (define (wait-for-idle-session a-session-queue)
    (let ([tau (get-sleep-time a-session-queue)])
      (and tau
           (or (<= tau 0)
               (begin
                 (sleep tau) (wait-for-idle-session a-session-queue))))))
  
  ;; get-sleep-time: sessoin-queue -> (union number #f)
  ;; get the time to sleep or #f if the queue is empty
  ;; if the time is negative then kill the idle session befor returning
  (define (get-sleep-time a-session-queue)
    (session-queue-atomic-action
     a-session-queue
     (lambda ()
       (and (not (empty-session-queue? a-session-queue))
            (let ([tau (time-to-live (peek-session a-session-queue))])
              (when (<= tau 0)
                (kill-sessions! a-session-queue))
              tau)))))
  
  ;; kill-sessions! session-queue ->
  ;; dequeue all the idle sessions and kill half of them
  ;; push the survivors back into the queue.
  (define (kill-sessions! a-session-queue)
    (let ([idle-sessions
           (let loop ([tau (time-to-live (peek-session a-session-queue))])
             (if (> tau 0) '()
                 (cons (dequeue-session a-session-queue)
                       (if (empty-session-queue? a-session-queue) '()
                           (loop (time-to-live (peek-session a-session-queue)))))))])
      (let loop ([i-s idle-sessions])
        (unless (null? i-s)
          (kill-this-session! (car i-s))
          (unless (null? (cdr i-s))
            (push-session (cadr i-s) a-session-queue)
            (loop (cddr i-s)))))))
  
  ;; time-to-live: session -> number
  ;; determine how many seconds remain before this session is considered idle
  (define (time-to-live ses)
    (let ([elapsed-time (- (current-seconds) (session-start-time ses))])
      (- (session-time-to-live ses) elapsed-time)))
  
  ;; kill-session!: session -> void
  ;; kill the thread associated with the session, shutdown the custodian etc.
  (define (kill-this-session! ses)
    ((session-shutdown! ses)))
  
  ;; kill-session!: session -> void
  ;; extract a session from a queue and kill it
  (define (kill-session! ses)
    (let ([a-session-queue (session-queue ses)])
      (extract-session
       (lambda (s) (eq? ses s))
       a-session-queue)
      (kill-this-session! ses)))
    
  
  ;; kill-all-sessions!: session-queue -> void
  ;; kill all the sessions in a queue
  (define (kill-all-sessions! a-session-queue)
    (session-queue-atomic-action
     a-session-queue
     (lambda ()
       (let loop ()
         (when (not (empty-session-queue? a-session-queue))
           (kill-this-session! (dequeue-session a-session-queue))
           (loop))))))
  
  ;; renew-sesssion!: session number
  ;; renew the start time of a session and move to the end of the queue
  (define renew-session!
    (case-lambda
      [(ses new-time-to-live)
       (let ([a-session-queue (session-queue ses)])
         (session-queue-atomic-action
          a-session-queue
          (lambda ()
            (extract-session
             (lambda (s) (eq? ses s))
             a-session-queue)
            (set-session-start-time! ses (current-seconds))
            (set-session-time-to-live! ses new-time-to-live)
            (enqueue-session ses a-session-queue))))]
      [(ses a-session-queue)
       (renew-session! ses (session-time-to-live ses))]))
  
  ;; make-session-collection: number -> (values procedure? procedure?)
  (define (make-session-collection session-queue-memory-threshold)
    (let ([a-session-queue (new-session-queue session-queue-memory-threshold)])
      (values
       
       ;; create-session: number alpha (->) -> (union (session-of alpha) #f)
       (lambda (time-to-live payload kill-thunk)
         (let ([res (can-create-session? a-session-queue)])
           (and res
                (let ([ses (make-session (current-seconds) time-to-live payload kill-thunk a-session-queue)])
                  (enqueue-session ses a-session-queue)
                  ses))))

       ;; kill-all-sessions!: -> void
       (lambda ()
         (kill-all-sessions! a-session-queue)))))
  )

