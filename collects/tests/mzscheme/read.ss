
(load-relative "loadtest.ss")

(SECTION 'READING)
(define readstr
  (lambda (s)
    (let* ([o (open-input-string s)]
	   [read (lambda () (read o))])
      (let loop ([last eof])
	(let ([v (read)])
	  (if (eof-object? v)
	      last
	      (loop v)))))))

(define readerrtype
  (lambda (x) x))

; Make sure {whitespace} == {delimiter}
(let ([with-censor (load-relative "censor.ss")])
  (with-censor
   (lambda ()
     (let loop ([n 0])
       (unless (= n 256)
	       (let* ([c0 (integer->char n)]
		      [c (if (read-case-sensitive)
			     c0
			     (char-downcase c0))])
		 (cond
		  [(char-whitespace? c)
		   (test 'b readstr (string #\a c #\b))]
		  [(char=? #\\ c) (test 'ab readstr (string #\a c #\b))]
		  [(char=? #\; c) (test 'a readstr (string #\a c #\b))]
		  [(char=? #\' c) (test ''b readstr (string #\a c #\b))]
		  [(char=? #\` c) (test '`b readstr (string #\a c #\b))]
		  [(char=? #\, c) (test ',b readstr (string #\a c #\b))]
		  [else
		   (test (string->symbol (string #\a (char-downcase c) #\b))
			 'readstr
			 (with-handlers ([void 
					  (lambda (x) 
					    (string->symbol (string #\a (char-downcase c) #\b)))])
			      (readstr (string #\a c #\b))))]))
	       (loop (add1 n)))))))

(err/rt-test (readstr ")") (readerrtype exn:read?))
(err/rt-test (readstr "[)") (readerrtype exn:read?))
(err/rt-test (readstr "[}") (readerrtype exn:read?))
(err/rt-test (readstr "8 )") (readerrtype exn:read?))
(err/rt-test (readstr "(8 . )") (readerrtype exn:read?))

(load-relative "numstrs.ss")
(let loop ([l number-table])
  (unless (null? l)
	  (let* ([pair (car l)]
		 [v (car pair)]
		 [s (cadr pair)])
	    (cond
	     [(eq? v 'X) (err/rt-test (readstr s) (readerrtype exn:read?))]
	     [v (test v readstr s)]
	     [else (test (string->symbol s) readstr s)]))
	  (loop (cdr l))))

(err/rt-test (readstr "#\\silly") (readerrtype exn:read?))
(err/rt-test (readstr "#\\nully") (readerrtype exn:read?))
(err/rt-test (readstr "#\\nu") (readerrtype exn:read?))
(err/rt-test (readstr "#\\733") (readerrtype exn:read?))
(err/rt-test (readstr "#\\433") (readerrtype exn:read?))

(err/rt-test (readstr "(hi") (readerrtype exn:read:eof?))
(err/rt-test (readstr "\"hi") (readerrtype exn:read:eof?))
(err/rt-test (readstr "#(hi") (readerrtype exn:read:eof?))
(err/rt-test (readstr "#4(hi") (readerrtype exn:read:eof?))
(err/rt-test (readstr "|hi") (readerrtype exn:read:eof?))
(err/rt-test (readstr "#\\") (readerrtype exn:read:eof?))
(err/rt-test (readstr "#| hi") (readerrtype exn:read:eof?))

(err/rt-test (readstr ".") (readerrtype exn:read?))
(err/rt-test (readstr "a .") (readerrtype exn:read?))
(err/rt-test (readstr "a . b") (readerrtype exn:read?))
(err/rt-test (readstr "( . )") (readerrtype exn:read?))
(err/rt-test (readstr "( . 8)") (readerrtype exn:read?))
(err/rt-test (readstr "(0 . 8 9)") (readerrtype exn:read?))
(err/rt-test (readstr "( . 8 9)") (readerrtype exn:read?))
(err/rt-test (readstr "(1 . 2 3 . 4)") (readerrtype exn:read?))
(err/rt-test (readstr "(1 . 2 . 3 . 4)") (readerrtype exn:read?))
(err/rt-test (readstr "#(8 . )") (readerrtype exn:read?))
(err/rt-test (readstr "#( . )") (readerrtype exn:read?))
(err/rt-test (readstr "#( . 8)") (readerrtype exn:read?))
(err/rt-test (readstr "#(0 . 8 9)") (readerrtype exn:read?))
(err/rt-test (readstr "#( . 8 9)") (readerrtype exn:read?))
(err/rt-test (readstr "#( 8 . 9)") (readerrtype exn:read?))
(err/rt-test (readstr "#( 8 . (9))") (readerrtype exn:read?))
(err/rt-test (readstr "#(1 . 2 . 3)") (readerrtype exn:read?))

(err/rt-test (readstr "#Q") (readerrtype exn:read?))
(err/rt-test (readstr "##") (readerrtype exn:read?))
(err/rt-test (readstr "#?") (readerrtype exn:read?))
(err/rt-test (readstr "#-1()") (readerrtype exn:read?))
(err/rt-test (readstr "#<a>") (readerrtype exn:read?))

(test '(1 2 3) readstr "(2 . 1 . 3)")
(test '(1 2 3 4) readstr "(2 . 1 . 3 4)")
(test '(1 2 3 4) readstr "(2 3 . 1 . 4)")

(test 2 vector-length (readstr "#2()"))
(test 0 vector-ref (readstr "#2()") 1)
(test 2 vector-length (readstr "#000000000000000000000000000000002()"))

(err/rt-test (readstr "#2(1 2 3)") (readerrtype exn:read?))
(err/rt-test (readstr "#200000000000(1 2 3)") (readerrtype exn:misc:out-of-memory?))

(test #t (lambda (x) (eq? (car x) (cdr x))) (readstr "(#1=(1 2) . #0001#)"))

(err/rt-test (readstr "#0#") (readerrtype exn:read?))
(err/rt-test (readstr "#0=#0#") (readerrtype exn:read?))
(err/rt-test (readstr "(#0# #0=7)") (readerrtype exn:read?))
(err/rt-test (readstr "(#0=7 #1#)") (readerrtype exn:read?))
(err/rt-test (readstr "#012345678=7") (readerrtype exn:read?))
(err/rt-test (readstr "(#12345678=7 #012345678#)") (readerrtype exn:read?))

(test 3 string-length (readstr (string #\" #\a #\nul #\b #\")))
(test (string->symbol (string #\a #\nul #\b)) 'sym (readstr (string #\a #\nul #\b)))
(test (string->symbol (string #\1 #\nul #\b)) 'sym (readstr (string #\1 #\nul #\b)))

; Test read/write invariance on symbols and use of pipe quotes
(define (test-write-sym with-bar without-bar s)
  (let ([sym (string->symbol s)])
    (parameterize ([read-case-sensitive #t])
		  (let ([p (open-output-string)])
		    (write sym p)
		    (test with-bar 'write-sym-with-bar (get-output-string p))
		    (test sym read (open-input-string (get-output-string p))))
		  (let ([p (open-output-string)])
		    (parameterize ([read-accept-bar-quote #f])
				  (write sym p)
				  (test without-bar 'write-sym-no-bar (get-output-string p))
				  (test sym read (open-input-string (get-output-string p)))))
		  (let ([p (open-output-string)])
		    (display sym p)
		    (test s 'display-sym (get-output-string p))))))

(test-write-sym "a->b" "a->b" "a->b")
(test-write-sym "|a,b|" "a\\,b" "a,b")
(test-write-sym "a\\|b" "a|b" "a|b")
(test-write-sym "|a\\b|" "a\\\\b" "a\\b")

(load-relative "numstrs.ss")
(let loop ([l number-table])
  (cond
   [(null? l) 'done]
   [(or (number? (caar l)) (eq? (caar l) 'X))
    (test-write-sym (string-append "|" (cadar l) "|") 
		    (string-append "\\" (cadar l)) 
		    (cadar l))
    (loop (cdr l))]
   [else 
    (test-write-sym (cadar l) (cadar l) (cadar l))
    (loop (cdr l))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test #$

(define-struct special (size))

(define a-special (make-special 7))
(define b-special (make-special 19))

(define (make-p stream special-size)
  ;; The `stream' arg is a list of strings and non-strings;
  ;;  characters from the strings are returned one by one,
  ;;  and the non-strings are returns as "special" literals.
  ;; The `special-size' arg meansures the size (in char
  ;;  positions) of a non-string special literal.
  (let* ([special-ready #f]
	 [pos 0]
	 [incpos! (lambda () (set! pos (add1 pos)))])
    (make-input-port
     ;; Read char:
     (lambda ()
       (when special-ready
	 (error "#$ result followed not by a request for special"))
       (let loop ([s stream][p pos])
	 (if (null? s)
	     eof
	     (let ([i (car s)])
	       (if (string? i)
		   (if ((string-length i) . > . p)
		       (begin
			(incpos!)
			(string-ref i p))
		       (loop (cdr s) (- p (string-length i))))
		   ;; a special:
		   (cond
		    [(zero? p) (incpos!) #\#]
		    [(= p 1) (incpos!) (set! special-ready i) #\$]
		    [else (loop (cdr s) (- p 2))]))))))
     ;; Char ready?
     (lambda () #t)
     ;; Close proc
     (lambda () #t)
     ;; Peek proc
     #f
     ;; Get special
     (lambda ()
       (unless special-ready
	 (error "special request when no special is ready"))
       (begin0
	(values special-ready (special-size special-ready))
	(set! special-ready #f))))))

;; Simple read:
(let* ([p (make-p `("(list "
		    ,a-special
		    " "
		    ,b-special
		    "))")
		  special-size)]
       [v (read p)])
  (test 'list car v)
  (test a-special cadr v)
  (test b-special caddr v))

;; Read with src loc:
(let* ([p (make-p `("(list "
		    ,a-special
		    " "
		    ,b-special
		    " end))")
		  special-size)]
       [v (read-syntax 'dk p)]
       [l (syntax->list v)]
       [v2 (syntax-object->datum v)])
  (test 'list car v2)
  (test a-special cadr v2)
  (test b-special caddr v2)
  (test 'end cadddr v2)
  
  (test 2 syntax-position (car l))
  (test 7 syntax-position (cadr l))
  (test 15 syntax-position (caddr l))
  (test 35 syntax-position (cadddr l))

  ;; Read with specials as syntax syntax already:
  (let* ([stx v]
	 [p (make-p `("(list "
		      ,stx
		      " end))")
		    (lambda (x)
		      ;; pretend it's 100 wide
		      100))]
	 [v (read-syntax 'dk p)]
	 [l (syntax->list v)])
    ;; make sure syntax object is intact:
    (test stx cadr l)
    (test 108 syntax-position (caddr l))

    ;; Check that plain read performs a syntax-object->datum:
    (let* ([p (make-p `("(list "
			,stx
			" end))")
		      (lambda (x) 100))]
	   [v (read p)])
      (test `(list (list ,a-special ,b-special end) end) values v))))

;; Check that syntax read with with a list special
;;  syntaxizes the list.
(let* ([p (make-p `("(list "
		    ,(list a-special b-special)
		    " end))")
		  (lambda (x)
		    100))]
       [v (read-syntax 'dk p)]
       [l (syntax->list v)])
  (test #t syntax? (cadr l))
  (test #t list? (syntax-e (cadr l)))
  (test a-special syntax-e (car (syntax-e (cadr l))))
  (test b-special syntax-e (cadr (syntax-e (cadr l))))
  (test 108 syntax-position (caddr l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test read-syntax offsets:

(let ([p (open-input-string " a ")])
  (let ([v (read-syntax 'ok p (list 70 700 7000))])
    (test #f syntax-line v)
    (test #f syntax-column v)
    (test 7002 syntax-position v)))

(let ([p (open-input-string " a ")])
  (port-count-lines! p)
  (let ([v (read-syntax 'ok p (list 70 700 7000))])
    (test 71 syntax-line v)
    (test 702 syntax-column v)
    (test 7002 syntax-position v)))

(let ([p (open-input-string " \n a ")])
  (port-count-lines! p)
  (let ([v (read-syntax 'ok p (list 70 700 7000))])
    (test 72 syntax-line v)
    (test 2 syntax-column v)
    (test 7004 syntax-position v)))

;; Check exception record:
(let ([p (open-input-string " . ")])
  (let ([x (with-handlers ([values values])
	     (read-syntax 'ok p (list 70 700 7000)))])
    (test p exn:read-port x)
    (test 'ok exn:read-source x)
    (test #f exn:read-line x)
    (test #f exn:read-column x)
    (test 7002 exn:read-position x)))
    
(let ([p (open-input-string " . ")])
  (port-count-lines! p)
  (let ([x (with-handlers ([values values])
	     (read-syntax 'ok p (list 70 700 7000)))])
    (test p exn:read-port x)
    (test 'ok exn:read-source x)
    (test 71 exn:read-line x)
    (test 702 exn:read-column x)
    (test 7002 exn:read-position x)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
