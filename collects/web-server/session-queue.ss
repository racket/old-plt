(module session-queue mzscheme
  
  (provide new-session-queue
           session-queue-memory-threshold
           session-queue?
           empty-session-queue?
           peek-session
           session-queue-atomic-action
           enqueue-session
           push-session
           dequeue-session
           extract-session)
  
  (define-struct queue (memory-threshold head tail channel thread))
  
  (define session-queue-memory-threshold queue-memory-threshold)
  
  ;; session-queue?: alpha -> boolean
  (define session-queue? queue?)
  
  ;; empty-session-queue?: queue -> boolean
  ;; just like queue-empty? but use session-queue-atomic-action
  (define (empty-session-queue? a-q)
    (session-queue-atomic-action
     a-q
     (lambda () (queue-empty? a-q))))
  
  ;; peek-session: queue -> alpha
  ;; just like peek but use session-queue-atomic-action
  (define (peek-session a-q)
    (session-queue-atomic-action
     a-q
     (lambda () (peek a-q))))
  
  
  ;; enqueue-session: alpha queue ->
  ;; just like enqueue but use session-queue-atomic-action
  (define (enqueue-session val a-q)
    (session-queue-atomic-action
     a-q
     (lambda () (enqueue val a-q))))
  
  ;; push-session: alpha queue ->
  ;; just like push but use session-queue-atomic-action
  (define (push-session val a-q)
    (session-queue-atomic-action
     a-q
     (lambda () (push val a-q))))
  
  ;; dequeue-session: queue -> alpha
  ;; just like dequeue but use session-queue-atomic-action
  (define (dequeue-session a-q)
    (session-queue-atomic-action
     a-q
     (lambda () (dequeue a-q))))
  
  ;; extract-session: (alpha -> boolean) (queue-of alpha) -> (union alpha #f)
  (define (extract-session pred? a-q)
    (session-queue-atomic-action
     a-q
     (lambda () (queue-extract pred? a-q))))
  
  ;; new-session-queue number -> queue
  (define (new-session-queue memory-threshold)
    (let ([chan (make-channel)])
      (let ([q-thread
             (thread
              (lambda ()
                (let loop ()
                  ((channel-get chan))
                  (loop))))])
        (make-queue memory-threshold '() '() chan q-thread))))
  
  ;; session-queue-atomic-action: queue (-> alpha) -> alpha
  ;; the thunk is executed atomically by the queue's internal thread
  (define (session-queue-atomic-action a-q thunk)
    (cond
      [(eq? (queue-thread a-q) (current-thread)) (thunk)]
      [else
       (let ([chan (queue-channel a-q)]
             [chan-result (make-channel)])
         (channel-put chan
                      (lambda ()
                        (channel-put chan-result
                                     (with-handlers ([exn? (lambda (the-exn) the-exn)])
                                       (thunk)))
                        ))
         (let ([result (channel-get chan-result)])
           (when (exn? result)
             (raise result))
           result))]))
  
  
  ;; queue-empty? queue -> boolean
  (define (queue-empty? a-q)
    (null? (queue-head a-q)))
  
  ;; dequeue: queue -> value
  (define (dequeue a-q)
    (when (queue-empty? a-q) (error "dequeue: queue is empty"))
    (begin0
      (car (queue-head a-q))
      (set-queue-head! a-q (cdr (queue-head a-q)))))
  
  ;; peek: queue -> value
  (define (peek a-q)
    (when (queue-empty? a-q) (error "dequeue: queue is empty"))
    (car (queue-head a-q)))
  
  ;; enqueue: queue value -> void
  ;; enqueue at the tail
  (define (enqueue val a-q)
    (set-queue-head! a-q (append (queue-head a-q) (list val))))
  
  ;; push: queue value -> void
  ;; push at the front
  (define (push val a-q)
    (set-queue-head! a-q (cons val (queue-head a-q))))
  
  ;; queue-extract-session: (alpha -> boolean) (queue-of alpha) -> (union alpha #f)
  ;; find the session in the queue, based on the provided predicated, then remove the session
  (define (queue-extract pred? a-q)
    (let ([result #f])
      (set-queue-head! a-q
                       (let loop ([l (queue-head a-q)])
                         (cond
                           [(null? l) '()]
                           [(pred? (car l))
                            (set! result (car l))
                            (cdr l)]
                           [else (cons (car l) (loop (cdr l)))])))
      result))
  )
