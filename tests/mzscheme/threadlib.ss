
(load-relative "loadtest.ss")

(SECTION 'thread)

(require (lib "thread.ss"))

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

	   
  