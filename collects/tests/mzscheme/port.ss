
(load-relative "loadtest.ss")

(SECTION 'PORT)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Based on the MzScheme manual...

;; A port with no input...
;; Easy: \scheme{(open-input-bytes #"")}
;; Hard:
(define /dev/null-in 
  (make-input-port 'null
		   (lambda (s) eof)
		   (lambda (skip s) eof)
		   void
		   (lambda () always-evt)
		   (lambda (k progress-evt done-evt) #f)))
(test eof read-char /dev/null-in)
(test eof peek-char /dev/null-in)
(test eof read-byte-or-special /dev/null-in)
(test eof peek-byte-or-special /dev/null-in 100)
(test #t evt? (port-progress-evt /dev/null-in))
(test #t evt? (sync/timeout 0 (port-progress-evt /dev/null-in)))
(test #f port-commit-peeked 100 (port-progress-evt /dev/null-in) always-evt /dev/null-in)
(err/rt-test (port-commit-peeked 100 never-evt always-evt /dev/null-in))

;; A port that produces a stream of 1s:
(define infinite-ones 
  (make-input-port
   'ones
   (lambda (s) 
     (bytes-set! s 0 (char->integer #\1)) 1)
   #f
   void))
(test "11111" read-string 5 infinite-ones)

;; An infinite stream of 1s with a specific peek procedure:
(define infinite-ones 
  (let ([one! (lambda (s) 
	        (bytes-set! s 0 (char->integer #\1)) 1)])
    (make-input-port
     'ones
     one!
     (lambda (s skip) (one! s))
     void)))
(test "11111" read-string 5 infinite-ones)

;; Now we can peek ahead arbitrarily far:
(test "11111" peek-string 5 (expt 2 5000) infinite-ones)

;; The port doesn't supply procedures to implement progress events:
(test #f port-provides-progress-evts? infinite-ones)
(err/rt-test (port-progress-evt infinite-ones) exn:application:mismatch?)

;; This port produces 0, 1, 2, 0, 1, 2, etc,
;;  but it is not thread-safe, because multiple
;;  threads might read and change n
(define mod3-cycle/one-thread
  (let* ([n 2]
	 [mod! (lambda (s delta)
		 (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
                 1)])
    (make-input-port
     'mod3-cycle/not-thread-safe
     (lambda (s) 
       (set! n (modulo (add1 n) 3))
       (mod! s 0))
     (lambda (s skip) 
       (mod! s skip))
     void)))
(test "01201" read-string 5 mod3-cycle/one-thread)
(test "20120" peek-string 5 (expt 2 5000) mod3-cycle/one-thread)

;; Same thing, but thread-safe and kill-safe, and with progress
;; events. Only the server thread touches the stateful part
;; directly. (See the output port examples for a simpler thread-safe
;; example, but this one is more general.)
(define (make-mod3-cycle)
  (define read-req-ch (make-channel))
  (define peek-req-ch (make-channel))
  (define progress-req-ch (make-channel))
  (define commit-req-ch (make-channel))
  (define close-req-ch (make-channel))
  (define closed? #f)
  (define n 0)
  (define progress-sema #f)
  (define (mod! s delta)
    (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
    1)
  (define (remq v l)
    (if (eq? (car l) v)
	(cdr l)
	(cons (car l) (remq (cdr l) v))))
  ;; ----------------------------------------
  ;; The server has a list of outstanding commit requests,
  ;;  and it also must service each port operation (read, 
  ;;  progress-evt, etc.)
  (define (serve commit-reqs response-evts)
    (apply
     sync
     (finish-evt read-req-ch (handle-read commit-reqs response-evts))
     (finish-evt progress-req-ch (handle-progress commit-reqs response-evts))
     (finish-evt commit-req-ch (add-commit commit-reqs response-evts))
     (finish-evt close-req-ch (handle-close commit-reqs response-evts))
     (append
      (map (make-handle-response commit-reqs response-evts) response-evts)
      (map (make-handle-commit commit-reqs response-evts) commit-reqs))))
  ;; Read/peek request: fill in the string and commit
  (define ((handle-read commit-reqs response-evts) r)
    (let ([s (car r)]
	  [skip (cadr r)]
	  [ch (caddr r)]
	  [nack (cadddr r)]
	  [peek? (cddddr r)])
      (unless closed?
	(mod! s skip)
	(unless peek?
	  (commit! 1)))
      ;; Add an event to respond:
      (serve commit-reqs
	     (cons (choice-evt nack
			       (channel-put-evt ch (if closed? 0 1)))
		   response-evts))))
  ;; Progress request: send a peek evt for the current 
  ;;  progress-sema
  (define ((handle-progress commit-reqs response-evts) r)
    (let ([ch (car r)]
	  [nack (cdr r)])
      (unless progress-sema
	(set! progress-sema (make-semaphore (if closed? 1 0))))
      ;; Add an event to respond:
      (serve commit-reqs
	     (cons (choice-evt nack
			       (channel-put-evt
				ch
				(semaphore-peek-evt progress-sema)))
		   response-evts))))
  ;; Commit request: add the request to the list
  (define ((add-commit commit-reqs response-evts) r)
    (serve (cons r commit-reqs) response-evts))
  ;; Commit handling: watch out for progress, in which case
  ;;  the response is a commit failure; otherwise, try
  ;;  to sync for a commit. In either event, remove the
  ;;  request from the list
  (define ((make-handle-commit commit-reqs response-evts) r)
    (let ([k (car r)]
	  [progress-evt (cadr r)]
	  [done-evt (caddr r)]
	  [ch (cadddr r)]
	  [nack (cddddr r)])
      ;; Note: we don't check that k is $\leq$ the sum of
      ;;  previous peeks, because the entire stream is actually
      ;;  known, but we could send an exception in that case.
      (choice-evt
       (finish-evt progress-evt
		   (lambda (x) 
		     (sync nack (channel-put-evt ch #f))
		     (serve (remq r commit-reqs) response-evts)))
       ;; Only create an event to satisfy done-evt if progress-evt
       ;;  isn't already ready.
       ;; Afterward, if progress-evt becomes ready, then this
       ;;  event-making function will be called again.) We know this
       ;;  only because the server control all posts to progress-evt.
       (if (sync/timeout 0 progress-evt)
	   never-evt
	   (finish-evt done-evt
		       (lambda (v)
			 (commit! k)
			 (sync nack (channel-put-evt ch #t))
			 (serve (remq r commit-reqs) response-evts)))))))
  ;; Response handling: as soon as the respondee listerns,
  ;;  remove the response
  (define ((make-handle-response commit-reqs response-evts) evt)
    (finish-evt evt
		(lambda (x)
		  (serve commit-reqs
			 (remq evt response-evts)))))
  ;; Close handling: post the progress sema, if any, and set
  ;;   the \scheme{closed?} flag
  (define ((handle-close commit-reqs response-evts) r)
    (let ([ch (car r)]
	  [nack (cdr r)])
      (set! closed? #t)
      (when progress-sema
	(semaphore-post progress-sema))
      (serve commit-reqs
	     (cons (choice-evt nack
			       (channel-put-evt ch (void)))
		   response-evts))))
  ;; Helper for reads and post-peek commits:
  (define (commit! k)
    (when progress-sema
      (semaphore-post progress-sema)
      (set! progress-sema #f))
    (set! n (+ n k)))
  ;; Start the server thread:
  (define server-thread (thread (lambda () (serve null null))))
  ;; ----------------------------------------
  ;; Client-side helpers:
  (define (req-evt f)
    (nack-guard-evt
     (lambda (nack)
       ;; Be sure that the server thread is running:
       (thread-resume server-thread (current-thread))
       ;; Create a channel to hold the reply:
       (let ([ch (make-channel)])
	 (f ch nack)
	 ch))))
  (define (read-or-peek-evt s skip peek?)
    (req-evt (lambda (ch nack)
	       (channel-put read-req-ch (list* s skip ch nack peek?)))))
  ;; Make the port:
  (make-input-port 'mod3-cycle
		   ;; Each handler for the port just sends
		   ;;  a request to the server
		   (lambda (s) (read-or-peek-evt s 0 #f))
		   (lambda (s skip) (read-or-peek-evt s skip #t))
		   (lambda () ; close
		     (sync (req-evt
			    (lambda (ch nack)
			      (channel-put progress-req-ch (list* ch nack))))))
		   (lambda () ; progress-evt
		     (sync (req-evt
			    (lambda (ch nack)
			      (channel-put progress-req-ch (list* ch nack))))))
		   (lambda (k progress-evt done-evt)  ; commit
		     (sync (req-evt
			    (lambda (ch nack)
			      (channel-put commit-req-ch
					   (list* k progress-evt done-evt ch nack))))))))

(let ([mod3-cycle (make-mod3-cycle)])
  (port-progress-evt mod3-cycle)
  (let ([result1 #f]
	[result2 #f])
    (let ([t1 (thread (lambda ()
			(set! result1 (read-string 5 mod3-cycle))))]
	  [t2 (thread (lambda ()
			(set! result2 (read-string 5 mod3-cycle))))])
      (thread-wait t1)
      (thread-wait t2)
      (test 11 string-length (string-append result1 "," result2))))
  (let ([s (make-bytes 1)]
	[progress-evt (port-progress-evt mod3-cycle)])
    (peek-bytes-avail! s 0 mod3-cycle)
    (test #"1" values s)
    (test #t 
	  port-commit-peeked 1 progress-evt (make-semaphore 1)
	  mod3-cycle)
    (test #t evt? (sync/timeout 0 progress-evt))
    (test #f 
	  port-commit-peeked 1 progress-evt (make-semaphore 1) 
	  mod3-cycle))
  (close-input-port mod3-cycle))

;; Non-byte port results:
(define infinite-voids
  (make-input-port
   'voids
   (lambda (s) (lambda args 'void))
   (lambda (skip s) (lambda args 'void))
   void))
(err/rt-test (read-char infinite-voids) exn:application:mismatch?)
(test 'void read-char-or-special infinite-voids)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Also based on the MzScheme manual...

(define should-be-breakable? #t)

(define /dev/null-out
  (make-output-port 
   'null
   always-evt
   (lambda (s start end non-block? breakable?)
     (test should-be-breakable? 'breakable breakable?)
     (- end start))
   void
   (lambda (special non-block? breakable?) 
     (test should-be-breakable? 'spec-breakable breakable?)
     #t)
   (lambda (s start end) (convert-evt
			  always-evt
			  (lambda (x)
			    (- end start))))
   (lambda (special) always-evt)))
(test (void) display "hello" /dev/null-out)
(test 5 write-bytes-avail #"hello" /dev/null-out)
(test #t write-special 'hello /dev/null-out)
(test 5 sync (write-bytes-avail-evt #"hello" /dev/null-out))
(test 5 write-bytes-avail/enable-break #"hello" /dev/null-out)
(test #t write-special-avail* 'hello /dev/null-out)
(parameterize-break #f
  (test 5 write-bytes-avail/enable-break #"hello" /dev/null-out)
  (set! should-be-breakable? #f)
  (test #t write-special-avail* 'hello /dev/null-out)
  (test 5 write-bytes-avail #"hello" /dev/null-out))

;; A part that accumulates bytes as characters in a list,
;;  but not in a thread-safe way:
(define accum-list null)
(define accumulator/not-thread-safe
  (make-output-port 
   'accum/not-thread-safe
   always-evt
   (lambda (s start end non-block? breakable?)
     (set! accum-list
           (append accum-list
                   (map integer->char
                        (bytes->list (subbytes s start end)))))
     (- end start))
   void))
(display "hello" accumulator/not-thread-safe)
(test '(#\h #\e #\l #\l #\o) values accum-list)

;; Same as before, but with simple thread-safety:
(define accum-list null)
(define accumulator 
  (let* ([lock (make-semaphore 1)]
	 [lock-peek-evt (semaphore-peek-evt lock)])
    (make-output-port
     'accum
     lock-peek-evt
     (lambda (s start end non-block? breakable?)
       (if (semaphore-try-wait? lock)
           (begin
             (set! accum-list
                   (append accum-list
		           (map integer->char
                                (bytes->list (subbytes s start end)))))
             (semaphore-post lock)
             (- end start))
	   ;; Cheap strategy: block until the list is unlocked,
	   ;;   then return 0, so we get called again
           (convert-evt
	    lock-peek
	    (lambda (x) 0))))
     void)))
(display "hello" accumulator)
(test '(#\h #\e #\l #\l #\o) values accum-list)

;; A port that transforms data before sending it on
;;  to another port. Atomic writes exploit the
;;  underlying port's ability for atomic writes.
(define (make-latin-1-capitalize port)
  (define (byte-upcase s start end)
    (list->bytes
     (map (lambda (b) (char->integer
		       (char-upcase
			(integer->char b))))
	  (bytes->list (subbytes s start end)))))
  (make-output-port
   'byte-upcase
   ;; This port is ready when the original is ready:
   port
   ;; Writing procedure:
   (lambda (s start end non-block? breakable?)
     (let ([s (byte-upcase s start end)])
       (if non-block?
           (write-bytes-avail* s port)
           (begin
             (display s port)
             (bytes-length s)))))
   ;; Close procedure --- close original port:
   (lambda () (close-output-port port))
   #f
   ;; Write event:
   (and (port-writes-atomic? port)
	(lambda (s start end)
	  (write-bytes-avail-evt (byte-upcase s start end) port)))))
(define orig-port (open-output-string))
(define cap-port (make-latin-1-capitalize orig-port))
(display "Hello" cap-port)
(test "HELLO" get-output-string orig-port)
(test 3 sync (write-bytes-avail-evt #"Bye" cap-port))
(test "HELLOBYE" get-output-string orig-port)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
