
(load-relative "loadtest.ss")

(SECTION 'port)

(require (lib "port.ss"))

;; pipe and pipe-with-specials commmit tests
(define (test-pipe-commit make-pipe)
  (let-values ([(in out) (make-pipe)])
    (display "apple" out)
    (test #"app" peek-bytes 3 0 in)
    (let ([sema (make-semaphore 1)])
      (test #t port-commit-peeked 3 (port-progress-evt in) sema in)
      (test #f semaphore-try-wait? sema))
    (test #"le" read-bytes 2 in)
    (display "banana" out)
    (test #"ban" peek-bytes 3 0 in)
    ;; Set up a commit that fails, because the done-evt never becomes ready:
    (let* ([r '?]
	   [unless-evt (port-progress-evt in)]
	   [th (thread
		(lambda ()
		  (set! r (port-commit-peeked 3 unless-evt never-evt in))))])
      (sleep 0.01)
      (test #t thread-running? th)
      (test #\b peek-char in)
      (sleep 0.01)
      (test #t thread-running? th)
      (test #f sync/timeout 0 unless-evt)
      (test #\b read-char in)
      (sleep 0.01)
      (test th sync th)
      (test #f values r))
    (test "anana" read-string 5 in)
    ;; Set up two commits, pick one to succeed:
    (let ([go (lambda (which peek?)
		(display "donut" out)
		(test #"don" peek-bytes 3 0 in)
		(let* ([r1 '?]
		       [r2 '?]
		       [s1 (make-semaphore)]
		       [s2 (make-semaphore)]
		       [unless-evt (port-progress-evt in)]
		       [th1 (thread
			     (lambda ()
			       (set! r1 (port-commit-peeked 1 unless-evt (wrap-evt s1 void) in))))]
		       [_ (sleep 0.01)]
		       [th2 (thread
			     (lambda ()
			       (set! r2 (port-commit-peeked 2 unless-evt (semaphore-peek-evt s2) in))))])
		  (sleep 0.01)
		  (test #t thread-running? th1)
		  (test #t thread-running? th2)
		  (when peek?
		    (test #"do" peek-bytes 2 0 in)
		    (sleep 0.01))
		  (unless (= which 3)
		    (semaphore-post (if (= which 1) s1 s2)))
		  (when (= which 3)
		    (test #"do" read-bytes 2 in))
		  (sleep 0.01)
		  (test unless-evt sync/timeout 0 unless-evt)
		  (test #f thread-running? th1)
		  (sleep 0.01)
		  (test #f thread-running? th2)
		  (test (= which 1) values r1)
		  (test (= which 2) values r2)
		  (test (if (= which 1) #\o #\n) read-char in)
		  (test (if (= which 1) #"nut" #"ut") read-bytes (if (= which 1) 3 2) in)))])
      (go 1 #f)
      (go 2 #f)
      (go 1 #t)
      (go 2 #t)
      (go 3 #f))))
(test-pipe-commit make-pipe)
(test-pipe-commit make-pipe-with-specials)


;; copy-port and make-pipe-with-specials tests
(let ([s (let loop ([n 10000][l null])
	   (if (zero? n)
	       (apply bytes l)
	       (loop (sub1 n) (cons (random 256) l))))])
  (let-values ([(in out) (make-pipe-with-specials)])
    (display s out)
    (test #t 'pipe-same? (bytes=? s (read-bytes (bytes-length s) in)))
    (test out sync/timeout 0 out)
    (test #f sync/timeout 0 in)
    (write-special 'hello? out)
    (test 'hello? read-char-or-special in)
    (display "123" out)
    (write-special 'again! out)
    (display "45" out)
    (let ([s (make-bytes 5)])
      (test 3 read-bytes-avail! s in)
      (test #"123\0\0" values s)
      (let ([p (read-bytes-avail! s in)])
	(test #t procedure? p)
	(test 'again! p 'ok 1 2 3))
      (test 2 read-bytes-avail! s in)
      (test #"453\0\0" values s)))
  (let ([in (open-input-bytes s)]
	[out (open-output-bytes)])
    (copy-port in out)
    (test #t 'copy-same? (bytes=? s (get-output-bytes out))))
  (let* ([a (subbytes s 0 (max 1 (random (bytes-length s))))]
	 [b (subbytes s (bytes-length a) (+ (bytes-length a)
					    (max 1 (random (- (bytes-length s) (bytes-length a))))))]
	 [c (subbytes s (+ (bytes-length a) (bytes-length b)))])
    (define (go-stream close? copy? threads? peek?)
      (printf "Go stream: ~a ~a ~a ~a~n" close? copy? threads? peek?)
      (let*-values ([(in1 out) (make-pipe-with-specials)]
		    [(in out1) (if copy?
				   (make-pipe-with-specials)
				   (values in1 out))])
	(let ([w-th
	       (lambda ()
		 (display a out)
		 (write-special '(first one) out)
		 (display b out)
		 (write-special '(second one) out)
		 (display c out)
		 (when close?
		   (close-output-port out)))]
	      [c-th (lambda ()
		      (when copy?
			(copy-port in1 out1)
			(close-output-port out1)))]
	      [r-th (lambda ()
		      (let ([get-one-str
			     (lambda (a)
			       (let ([dest (make-bytes (bytes-length s))]
				     [target (bytes-length a)])
				 (let loop ([n 0])
				   (let ([v (read-bytes-avail! dest in n)])
				     (if (= target (+ v n))
					 (test #t `(same? ,target) (equal? (subbytes dest 0 target) a))
					 (loop (+ n v)))))))]
			    [get-one-special
			     (lambda (spec)
			       (let ([v (read-bytes-avail! (make-bytes 10) in)])
				 (test #t procedure? v)
				 (test spec v 'ok 5 5 5)))])
			(when peek?
			  (test '(second one) peek-byte-or-special in (+ (bytes-length a) 1 (bytes-length b))))
			(get-one-str a)
			(get-one-special '(first one))
			(get-one-str b)
			(get-one-special '(second one))
			(get-one-str c)
			(if close?
			    (test eof read-byte in)
			    (test #f sync/timeout 0 in))))])
	  (let ([th (if threads?
			thread
			(lambda (f) (f)))])
	    (for-each (lambda (t)
			(and (thread? t) (thread-wait t)))
		      (list
		       (th w-th)
		       (th c-th)
		       (th r-th)))))))
    (go-stream #f #f #f #f)
    (go-stream #t #f #f #f)
    (go-stream #t #t #f #f)
    (go-stream #t #f #t #f)
    (go-stream #t #t #t #f)
    (go-stream #t #f #f #t)
    (go-stream #t #t #f #t)
    (go-stream #t #f #t #t)
    (go-stream #t #t #t #t)))

;; pipe-with-specials and limit
(let-values ([(in out) (make-pipe-with-specials 10)])
  (test 10 write-bytes-avail #"1234567890ab" out)
  (test #f sync/timeout 0 out)
  (test #"1234" read-bytes 4 in)
  (test out sync/timeout 0 out)
  (test 4 write-bytes-avail #"xyzwqrst" out)
  (test 0 write-bytes-avail* #"xyzwqrst" out)
  (test #t write-special 'ok out)
  ;; Now that we've written a special, text will go out, too
  (test 8 write-bytes-avail* #"xyzwqrst" out)
  (let ([s (make-bytes 40)])
    (test 10 read-bytes-avail! s in)
    (test #"567890xyzw" subbytes s 0 10))
  (test 'ok read-char-or-special in)
  (close-output-port out)
  (test #"xyzwqrst" read-bytes 40 in)
  (test eof read-bytes 40 in))

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
