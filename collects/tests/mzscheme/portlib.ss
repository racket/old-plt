
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
(define (go mk-hello sync atest btest)
  (test #t list? (list mk-hello sync atest btest))
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
    (atest '(#"hello") sync (regexp-match-evt #rx".*" p)))
  (let ([p (mk-hello)])
    (atest '(#"hel") sync (regexp-match-evt #rx"..." p))
    (atest '(#"lo") sync (regexp-match-evt #rx".." p)))
  (let ([p (mk-hello)])
    (atest #"hello" sync (read-bytes-line-evt p))
    (atest eof sync (read-bytes-line-evt p))
    (atest eof sync (eof-evt p)))
  (let ([p (mk-hello)])
    (atest "hello" sync (read-line-evt p))
    (atest eof sync (read-line-evt p))))
(go (lambda () (open-input-bytes #"hello")) sync test test)

(define (sync/poll . args) (apply sync/timeout 0 args))
(go (lambda () (open-input-bytes #"hello")) sync/poll test test)

(define (delay-hello)
  (let-values ([(r w) (make-pipe)])
    (thread (lambda ()
	      (sleep 0.1)
	      (write-string "hello" w)
	      (close-output-port w)))
    r))
(go delay-hello sync test test)

(go (lambda ()
      (let-values ([(r w) (make-pipe)])
	r))
    sync/poll 
    (lambda args
      (apply test #f (cdr args)))
    (lambda args
      (apply test (if (string? (car args))
		      (make-string (string-length (car args)))
		      (make-bytes (bytes-length (car args))))
	     (cdr args))))


;; extra checks for read-line-evt:
(let ([p (open-input-string "ab\nc")])
  (test "ab" sync (read-line-evt p))
  (test "c" sync (read-line-evt p))
  (test eof sync (read-line-evt p)))
(let ([p (open-input-string "ab\nc")])
  (test "ab\nc" sync (read-line-evt p 'return))
  (test eof sync (read-line-evt p 'return)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'return))
  (test "\nc" sync (read-line-evt p 'return))
  (test eof sync (read-line-evt p 'return)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'return-linefeed))
  (test "c\r" sync (read-line-evt p 'return-linefeed))
  (test eof sync (read-line-evt p 'return-linefeed)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'any))
  (test "c" sync (read-line-evt p 'any))
  (test eof sync (read-line-evt p 'any)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'any-one))
  (test "" sync (read-line-evt p 'any-one))
  (test "c" sync (read-line-evt p 'any-one))
  (test eof sync (read-line-evt p 'any-one)))

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

;; make-limited-input-port tests
(let* ([s (open-input-string "123456789")]
       [s2 (make-limited-input-port s 5)])
  (test #"123" peek-bytes 3 0 s2)
  (test #"12345" peek-bytes 6 0 s2)
  (test #"12" read-bytes 2 s2)
  (test #"345" read-bytes 6 s2)
  (test eof read-bytes 6 s2)
  (test #f port-provides-progress-evts? s2))
(let-values ([(i o) (make-pipe)])
  (let ([s (make-limited-input-port i 5)])
    (test #f char-ready? s)
    (display "123" o)
    (test #t char-ready? s)
    (let ([b (make-bytes 10)])
      (test 3 peek-bytes-avail!* b 0 #f s)
      (test 3 read-bytes-avail!* b s)
      (test 0 peek-bytes-avail!* b 0 #f s)
      (display "456" o)
      (test 2 peek-bytes-avail!* b 0 #f s)
      (test 1 peek-bytes-avail!* b 1 #f s)
      (test 2 read-bytes-avail!* b s))))
	     

(report-errs)
