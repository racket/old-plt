(require "../session-queue.ss")
 
(define my-q (new-session-queue 200000000))
(define another-q (new-session-queue 200000000))


(session-queue-atomic-action
 my-q
 (lambda ()
   (session-queue-atomic-action
    my-q
    (lambda ()
      (session-queue-atomic-action
       my-q
       (lambda () #t))))))

;; This deadlocks:
;(define (atomic-swap q1 q2)
;  (session-queue-atomic-action
;   q1
;   (lambda ()
;     (session-queue-atomic-action
;      q2
;      (lambda ()
;        (enqueue-session (dequeue-session q2) q1)
;        (enqueue-session (dequeue-session q1) q2))))))

;; this doesn't:
(define (atomic-swap q1 q2)
  (session-queue-atomic-action
   q1
   (lambda ()
     (let ([q1-val (dequeue-session q1)])
       (enqueue-session
        (session-queue-atomic-action
         q2
         (lambda ()
           (let ([q2-val (dequeue-session q2)])
             (enqueue-session q1-val q2)
             q2-val)))
        q1)))))

(enqueue-session 1 my-q)
(enqueue-session 2 another-q)
(atomic-swap my-q another-q)
(= 2 (dequeue-session my-q))
(= 1 (dequeue-session another-q))

(enqueue-session 1 my-q)
(enqueue-session 2 my-q)
(= 1 (extract-session (lambda (x) #t) my-q))
(= 2 (extract-session (lambda (x) #t) my-q))
(not (extract-session (lambda (x) #t) my-q))
(not (extract-session (lambda (x) #t) my-q))







(define (make-thread-loop id)
  (lambda ()
    (let ([my-q (new-session-queue 200000000)])
      (let loop ([i 0])
        (when (< i 25)
          (session-queue-atomic-action
           my-q
           (lambda ()
             (enqueue-session 1 my-q)
             (enqueue-session 2 my-q)
             (enqueue-session 3 my-q)
             (enqueue-session 4 my-q)
             (or (and (= 1 (dequeue-session my-q))
                      (= 3 (extract-session (lambda (x) (= x 3)) my-q))
                      (= 2 (dequeue-session my-q))
                      (= 4 (dequeue-session my-q))
                      (printf "thread ~a: success! (i = ~a)~n" id i))
                 (error (format "thread ~a: clobbered" id)))))
          (loop (add1 i)))))))

(define (start-threads)
  (let ([threads (map (lambda (id)
                        (thread (make-thread-loop id)))
                      '(1 2 3 4 5 6 7 8))])
    (lambda ()
      (map kill-thread threads))))

;; ********************************************************************************
(require "../session.ss")

(define q (new-session-queue 300000000))

(define make-ses
  (lambda ()
    (create-session q 20 (make-vector 1000 #f) (lambda () (printf "help me I'm dying!~n")))))

(define ses1 (make-ses))
(define ses2 (make-ses))
(define ses3 (make-ses))

(extract-session/kill! ses1 q)
(extract-session/kill! ses2 q)
(extract-session/kill! ses3 q)
(extract-session/kill! ses1 q)
(extract-session/kill! ses1 q)

  