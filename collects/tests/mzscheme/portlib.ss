
(load-relative "loadtest.ss")

(SECTION 'port)

(require (lib "port.ss"))

;; make-input-port/read-to-peek
(define (make-list-port . l)
  (make-input-port/read-to-peek 
   'list-port
   (lambda (bytes)
     (cond
      [(null? l) eof]
      [(byte? (car l))
       (bytes-set! bytes 0 (car l))
       (set! l (cdr l))
       1]
      [(and (char? (car l))
	    (byte? (char->integer (car l))))
       (bytes-set! bytes 0 (char->integer (car l)))
       (set! l (cdr l))
       1]
      [else
       (let ([v (car l)])
	 (set! l (cdr l))
	 (lambda (a b c d) v))]))
   #f
   void))

(let ([p (make-list-port #\h #\e #\l #\l #\o)])
  (test (char->integer #\h) peek-byte p)
  (test (char->integer #\e) peek-byte p 1)
  (test (char->integer #\l) peek-byte p 2)
  (test #"hel" read-bytes 3 p)
  (test (char->integer #\l) peek-byte p)
  (test (char->integer #\o) peek-byte p 1)
  (test #"lo" read-bytes 3 p)
  (test eof peek-byte p)
  (test eof peek-byte p)
  (test eof read-byte p)
  (test eof read-byte p))

(let ([p (make-list-port #\h #\e #\l 'ack #\l #\o)])
  (test (char->integer #\h) read-byte p)
  (test (char->integer #\e) read-byte p)
  (test (char->integer #\l) read-byte p)
  (test 'ack read-byte-or-special p)
  (test (char->integer #\l) read-byte p)
  (test (char->integer #\o) read-byte p))

(let ([p (make-list-port #\h #\e #\l 'ack #\l #\o)])
  (test (char->integer #\h) peek-byte p)
  (test (char->integer #\l) peek-byte p 2)
  (test 'ack peek-byte-or-special p 3)
  (test (char->integer #\l) peek-byte p 4)
  (test #"hel" read-bytes 3 p)
  (test 'ack read-byte-or-special p)
  (test #"lo" read-bytes 4 p))

(test 'hello read (make-list-port #\h #\e #\l #\l #\o))
(let ([p (make-list-port #\h #\e #\l eof #\l #\o)])
  (test 'hel read p)
  (test eof read p)
  (test 'lo read p)
  (test eof read p)
  (test eof read p))
(let ([p (make-list-port #\h #\e #\l #\u7238 #\l #\o)])
  (test 'hel read p)
  (test #\u7238 read p)
  (test 'lo read p))

;; read synchronization events
(define (go mk-hello sync atest btest ctest)
  (test #"" sync (read-bytes-evt 0 (mk-hello)))
  (let ([p (mk-hello)])
    (atest #"hello" sync (read-bytes-evt 5 p))
    (atest eof sync (read-bytes-evt 5 p)))
  (test 0 sync (read-bytes!-evt (make-bytes 0) (mk-hello)))
  (let ([s (make-bytes 5)]
	[p (mk-hello)])
    (atest 5 sync (read-bytes!-evt s p))
    (btest #"hello" values s)
    (atest eof sync (read-bytes!-evt s p)))
  (test 0 sync (read-bytes-avail!-evt (make-bytes 0) (mk-hello)))
  (let ([s (make-bytes 5)]
	[p (mk-hello)])
    (atest 5 sync (read-bytes-avail!-evt s p))
    (btest #"hello" values s)
    (atest eof sync (read-bytes-avail!-evt s p)))
  (test "" sync (read-string-evt 0 (mk-hello)))
  (let ([p (mk-hello)])
    (atest "hello" sync (read-string-evt 5 p))
    (atest eof sync (read-bytes-evt 5 p)))
  (test 0 sync (read-string!-evt (make-string 0) (mk-hello)))
  (let ([s (make-string 5)]
	[p (mk-hello)])
    (atest 5 sync (read-string!-evt s p))
    (btest "hello" values s)
    (atest eof sync (read-string!-evt s p)))
  (let ([p (mk-hello)])
    (atest '(#"hello") sync (regexp-match-evt #rx"....." p)))
  (let ([p (mk-hello)])
    (ctest '(#"hello") sync (regexp-match-evt #rx".*" p)))
  (let ([p (mk-hello)])
    (atest '(#"hel") sync (regexp-match-evt #rx"..." p))
    (atest '(#"lo") sync (regexp-match-evt #rx".." p))))
(go (lambda () (open-input-bytes #"hello")) sync test test test)

(define (sync/poll . args) (apply sync/timeout 0 args))
(go (lambda () (open-input-bytes #"hello")) sync/poll test test test)

(define (delay-hello)
  (let-values ([(r w) (make-pipe)])
    (thread (lambda ()
	      (sleep 0.1)
	      (write-string "hello" w)
	      (close-output-port w)))
    r))
(go delay-hello sync test test test)

(go delay-hello sync/poll 
    (lambda args
      (apply test #f (cdr args)))
    (lambda args
      (apply test (if (string? (car args))
		      (make-string (string-length (car args)))
		      (make-bytes (bytes-length (car args))))
	     (cdr args)))
    (lambda args
      (apply test '(#"") (cdr args))))

(report-errs)
done  

;; input-port-append tests
(let* ([do-test
	;; ls is a list of strings for ports
	;;  n, m, q are positive
	;;  n and n+m < total length
	;;  n+m+q can be greater than total length
	(lambda (ls n m q)
	  (let* ([p (apply input-port-append #f (map open-input-string ls))]
		 [s (apply string-append ls)]
		 [l (string-length s)])
	    (test (substring s 0 n) peek-string n 0 p)
	    (test (substring s n (min l (+ n m q))) peek-string (+ m q) n p)
	    (test (substring s (+ n m) (min l (+ n m q))) peek-string q (+ n m) p)

	    (test (substring s 0 n) read-string n p)
	    
	    (test (substring s n (+ n m)) peek-string m 0 p)
	    (test (substring s (+ n m) (min l (+ n m q))) peek-string q m p)

	    (test (substring s n (+ n m)) read-string m p)

	    (test (substring s (+ n m) (min l (+ n m q))) peek-string q 0 p)))]
       [do-tests
	(lambda (ls)
	  (let ([l (apply + (map string-length ls))])
	    (let loop ([n 1])
	      (unless (= n (- l 2))
		(let loop ([m 1])
		  (unless (= (+ m n) (- l 1))
		    (do-test ls n m 1)
		    (do-test ls n m (- l n m))
		    (do-test ls n m (+ (- l n m) 2))
		    (loop (add1 m))))
		(loop (add1 n))))))])
  (do-tests '("apple" "banana"))
  (do-tests '("ax" "b" "cz")))
   