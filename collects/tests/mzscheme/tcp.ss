
(define id 40001)

(define (client host)
  (lambda ()
    (tcp-connect host id)))
  
(define server
  (lambda ()
    (let ([l (tcp-listen id)])
      (tcp-accept l))))

(define (tread connect)
  (let-values ([(r w) (connect)])
    (printf "Hit return to start reading~n")
    (read-line)
    (let loop ([last -1])
      (let ([v (read r)])
	(if (eof-object? v)
	    last
	    (begin
	      (unless (= v (add1 last))
		(printf "skipped! ~a ~a~n" last v))
	      (when (zero? (modulo v 50000))
		(printf "~a~n" v))
	      (loop v)))))))

(define (twrite connect)
  (let-values ([(r w) (connect)])
    (let loop ([n 0])
      (if (tcp-port-send-waiting? w)
	  (begin
	    (printf "write-full at ~s~n" n)
	    (let loop ([m 0])
	      (if (= m 5)
		  (begin
		    (printf "done: ~s~n" (+ m n -1))
		    (close-output-port w)
		    (close-input-port r))
		  (begin
		    (fprintf w "~s~n" (+ m n))
		    (loop (add1 m))))))
	  (begin
	    (fprintf w "~s~n" n)
	    (loop (add1 n)))))))
