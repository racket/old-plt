(define semaphore (make-semaphore 0))
(define loop-size 1000)
(define events/loop 10)

(define frame (make-object frame% "frame" #f 100 100))
(define canvas
  (let ([counter events/loop])
    (make-object 
     (class canvas% ()
       (inherit refresh)
       (override
	[on-paint
	 (lambda ()
	   (cond
	     [(equal? 0 counter)
	      (set! counter #f)]
	     [else
	      (unless counter
		(set! counter events/loop))
	      (set! counter (- counter 1))
	      (refresh)]))])
       (sequence (super-init frame))))))

;(event-dispatch-handler (let ([orig (event-dispatch-handler)]) (lambda (eventspace) (orig eventspace))))

(define start-time (current-milliseconds))

(let loop ([n loop-size])
  (unless (zero? n)
    (send canvas on-paint)
    (loop (- n 1))))

(queue-callback (lambda () (semaphore-post semaphore)))
(yield semaphore)

(define end-time (current-milliseconds))

(define total-time (- end-time start-time))

(printf "time per event ~a msec~ntotal time ~a msec~n"
	(exact->inexact (/ (floor (* (/ total-time loop-size events/loop) 1000)) 1000))
	total-time)