(define semaphore (make-semaphore 0))
(define loop-size 2000)
(define events/loop 3)

;(event-dispatch-handler (let ([orig (event-dispatch-handler)]) (lambda (eventspace) (orig eventspace))))

(define start-time (current-milliseconds))

(let loop ([n loop-size])
  (unless (zero? n)
    (queue-callback void)
    (queue-callback void)
    (queue-callback void)
    (loop (- n 1))))

(queue-callback (lambda () (semaphore-post semaphore)))
(yield semaphore)

(define end-time (current-milliseconds))

(define total-time (- end-time start-time))

(define (mixed n)
  (cond
   [(<= -1 n 1) n]
   [(= n (floor n)) n]
   [else `(+ ,(floor n) ,(- n (floor n)))]))

(printf "time per event ~amsec~ntotal time ~amsec~n"
	(exact->inexact (/ (floor (* (/ total-time loop-size events/loop) 1000)) 1000))
	total-time)