
(load-relative "loadtest.ss")

(SECTION 'UDP)

(define udp1 (udp-open-socket))
(define us1 (make-string 10))

(test #t udp? udp1)
(test #f udp-bound? udp1)
(test #f udp-connected? udp1)

;; not bound:
(err/rt-test (udp-receive! udp1 us1) exn:i/o:udp?)
(err/rt-test (udp-receive!* udp1 us1) exn:i/o:udp?)
(err/rt-test (udp-receive!/enable-break udp1 us1) exn:i/o:udp?)
;; not connected:
(err/rt-test (udp-send udp1 us1) exn:i/o:udp?)
(err/rt-test (udp-send* udp1 us1) exn:i/o:udp?)
(err/rt-test (udp-send/enable-break udp1 us1) exn:i/o:udp?)

(test (void) udp-send-to udp1 "localhost" 45678 "knock knock")
(sleep 0.05)
(test-values '(#f #f #f) (lambda () 
			   ;; The send above might cause an error on the next
			   ;;  action, so we try up to 2 times:
			   (with-handlers ([not-break-exn?
					    (lambda (x)
					      (udp-receive!* udp1 us1))])
			     (udp-receive!* udp1 us1))))

(test #t udp-bound? udp1)
(test #f udp-connected? udp1)

;; still not connected:
(err/rt-test (udp-send udp1 us1) exn:i/o:udp?)

(define udp2 (udp-open-socket))
(test (void) udp-bind! udp2 #f 40007)
(test-values '(#f #f #f) (lambda () (udp-receive!* udp2 us1)))

(test (void) udp-send-to udp1 "localhost" 40007 "Hiya.")
(define recv-got (call-with-values (lambda () (udp-receive! udp2 us1)) list))
(test 5 car recv-got)
(test "127.0.0.1" cadr recv-got)
(define udp1-port (caddr recv-got))
(test "Hiya.\0\0\0\0\0" values us1)

(test (void) udp-send-to udp1 "localhost" 40007 "...another?..." 3 11)
(test-values (list 8 "127.0.0.1" udp1-port) (lambda () (udp-receive! udp2 us1 1)))
(test "Hanother?\0" values us1)

(test (void) udp-connect! udp1 "localhost" 40007)
(test #t udp-connected? udp1)
(test #f udp-connected? udp2)

(test (void) udp-send udp1 "truncate me")
(test-values (list 6 "127.0.0.1" udp1-port) (lambda () (udp-receive! udp2 us1 2 8)))
(test "Hatrunca?\0" values us1)

(test #t udp-send* udp1 "SKIPall of it" 4)
(test-values (list 9 "127.0.0.1" udp1-port) (lambda () (udp-receive! udp2 us1 0)))
(test "all of it\0" values us1)

(define (flush-udp-errors udp)
  (let loop ()
    (with-handlers ([not-break-exn?
		     (lambda (x) (loop))])
       (udp-receive!* udp1 us1))))

;; re-connect
(test (void) udp-connect! udp1 "localhost" 40008)
(test #t udp-connected? udp1)
(test (void) udp-send udp1 "lots of stuff")
(sleep 0.05)
(flush-udp-errors udp1)
(test-values '(#f #f #f) (lambda () (udp-receive!* udp2 us1)))
(test (void) udp-send-to udp1 "localhost" 40007 "ok, even if connected to something else")
(sleep 0.05)
(test-values (list 10 "127.0.0.1" udp1-port) (lambda () (udp-receive!* udp2 us1)))
(test "ok, even i" values us1)
(test #t udp-send* udp1 "lots of stuff")
(sleep 0.05)
(flush-udp-errors udp1)
(test-values '(#f #f #f) (lambda () (udp-receive!* udp2 us1)))

;; disconnect
(test (void) udp-connect! udp1 #f #f)
(test #f udp-connected? udp1)
(test (void) udp-connect! udp2 #f #f)
(test #f udp-connected? udp2)

(err/rt-test (udp-bind! udp1 #f #f))
(err/rt-test (udp-bind! udp1 "localhost" #f))
(err/rt-test (udp-connect! udp1 "localhost" #f) exn:application:mismatch?)
(err/rt-test (udp-connect! udp1 #f 5) exn:application:mismatch?)
(err/rt-test (udp-send-to udp1 #f 40000 "hello"))
(err/rt-test (udp-send-to udp1 "localhost" #f "hello"))
(err/rt-test (udp-send-to udp1 "localhost" 40000 'hello))
(err/rt-test (udp-send-to udp1 "localhost" 40000 "hello" #f))
(err/rt-test (udp-send-to udp1 "localhost" 40000 "hello" 1 #f))
(err/rt-test (udp-send-to udp1 "localhost" 40000 "hello" 10) exn:application:mismatch?)
(err/rt-test (udp-send-to udp1 "localhost" 40000 "hello" 1 11) exn:application:mismatch?)
(err/rt-test (udp-send-to udp1 "localhost" 40000 "hello" 1 0) exn:application:mismatch?)
(err/rt-test (udp-send udp1 'hello))
(err/rt-test (udp-send udp1 "hello" #f))
(err/rt-test (udp-send udp1 "hello" 2 #f))
(err/rt-test (udp-send udp1 "hello" 12) exn:application:mismatch?)
(err/rt-test (udp-send udp1 "hello" 1 13) exn:application:mismatch?)
(err/rt-test (udp-send udp1 "hello" 1 0) exn:application:mismatch?)
(err/rt-test (udp-receive! udp1 "constant"))
(err/rt-test (udp-receive! udp1 'hello))
(err/rt-test (udp-receive!* udp1 (make-string 10) #f))
(err/rt-test (udp-receive!* udp1 (make-string 10) 2 #f))
(err/rt-test (udp-receive!* udp1 (make-string 10) 12) exn:application:mismatch?)
(err/rt-test (udp-receive!* udp1 (make-string 10) 2 12) exn:application:mismatch?)
(err/rt-test (udp-receive!* udp1 (make-string 10) 2 0) exn:application:mismatch?)
