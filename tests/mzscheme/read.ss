
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

(err/rt-test (readstr ")") exn:read?)
(err/rt-test (readstr "[)") exn:read?)
(err/rt-test (readstr "[}") exn:read?)
(err/rt-test (readstr "8 )") exn:read?)
(err/rt-test (readstr "(8 . )") exn:read?)

(load-relative "numstrs.ss")
(let loop ([l number-table])
  (unless (null? l)
	  (let* ([pair (car l)]
		 [v (car pair)]
		 [s (cadr pair)])
	    (cond
	     [(eq? v 'X) 
	      (err/rt-test (readstr s) exn:read?)
	      (test #f string->number s)]
	     [v 
	      (test v readstr s)
	      (test (if (symbol? v) #f v) string->number s)]
	     [else 
	      (test (string->symbol s) readstr s)
	      (test #f string->number s)
	      (unless (regexp-match "#" s)
		(err/rt-test (readstr (string-append "#d" s)) exn:read?)
		(test #f string->number (string-append "#d" s)))]))
	  (loop (cdr l))))

(test 5 readstr "#| hi |# 5")
(test 5 readstr "#| #| #| #| hi |# |# |# |# 5")
(test '(5) readstr "(#| #| #| #| hi |# |# |# |# 5)")

(err/rt-test (readstr "#\\silly") exn:read?)
(err/rt-test (readstr "#\\nully") exn:read?)
(err/rt-test (readstr "#\\nu") exn:read?)
(err/rt-test (readstr "#\\733") exn:read?)
(err/rt-test (readstr "#\\433") exn:read?)
(err/rt-test (readstr "#\\longerthanthrityonecharcterswhichisthebufsize") exn:read?)
(err/rt-test (readstr "#\\rcase") exn:read?)
(err/rt-test (readstr "#\\pcase") exn:read?)
(err/rt-test (readstr "#\\tcase") exn:read?)
(err/rt-test (readstr "#\\vcase") exn:read?)
(err/rt-test (readstr "#\\bcase") exn:read?)
(err/rt-test (readstr "#\\lcase") exn:read?)

(err/rt-test (readstr "(hi") exn:read:eof?)
(err/rt-test (readstr "\"hi") exn:read:eof?)
(err/rt-test (readstr "\"hi\\") exn:read:eof?)
(err/rt-test (readstr "#(hi") exn:read:eof?)
(err/rt-test (readstr "#[hi") exn:read:eof?)
(err/rt-test (readstr "#{hi") exn:read:eof?)
(err/rt-test (readstr "#4(hi") exn:read:eof?)
(err/rt-test (readstr "#4[hi") exn:read:eof?)
(err/rt-test (readstr "#4{hi") exn:read:eof?)
(err/rt-test (readstr "|hi") exn:read:eof?)
(err/rt-test (readstr "hi\\") exn:read:eof?)
(err/rt-test (readstr "#\\") exn:read:eof?)
(err/rt-test (readstr "#\\12") exn:read:eof?)
(err/rt-test (readstr "#| hi") exn:read:eof?)
(err/rt-test (readstr "(1 #| hi") exn:read:eof?)
(err/rt-test (readstr "'") exn:read:eof?)
(err/rt-test (readstr "`") exn:read:eof?)
(err/rt-test (readstr ",@") exn:read:eof?)
(err/rt-test (readstr ",") exn:read:eof?)
(err/rt-test (readstr "#'") exn:read:eof?)
(err/rt-test (readstr "#&") exn:read:eof?)

(err/rt-test (readstr ".") exn:read?)
(err/rt-test (readstr "a .") exn:read?)
(err/rt-test (readstr "a . b") exn:read?)
(err/rt-test (readstr "( . )") exn:read?)
(err/rt-test (readstr "( . 8)") exn:read?)
(err/rt-test (readstr "(0 . 8 9)") exn:read?)
(err/rt-test (readstr "( . 8 9)") exn:read?)
(err/rt-test (readstr "(1 . 2 3 . 4)") exn:read?)
(err/rt-test (readstr "(1 . 2 . 3 . 4)") exn:read?)
(err/rt-test (readstr "#(8 . )") exn:read?)
(err/rt-test (readstr "#( . )") exn:read?)
(err/rt-test (readstr "#( . 8)") exn:read?)
(err/rt-test (readstr "#(0 . 8 9)") exn:read?)
(err/rt-test (readstr "#( . 8 9)") exn:read?)
(err/rt-test (readstr "#( 8 . 9)") exn:read?)
(err/rt-test (readstr "#( 8 . (9))") exn:read?)
(err/rt-test (readstr "#(1 . 2 . 3)") exn:read?)

(err/rt-test (readstr "#Q") exn:read?)
(err/rt-test (readstr "##") exn:read?)
(err/rt-test (readstr "#?") exn:read?)
(err/rt-test (readstr "#-1()") exn:read?)
(err/rt-test (readstr "#<a>") exn:read?)

(test #(1 a c) readstr "#[1 a c]")
(test #(1 a c) readstr "#{1 a c}")
(test #(1 a a) readstr "#3[1 a]")
(test #(1 a a) readstr "#3{1 a}")
(parameterize ([read-square-bracket-as-paren #f]
	       [read-curly-brace-as-paren #f]
	       [read-accept-quasiquote #f])
  (test '|[2| readstr "[2")
  (test '|{2| readstr "{2")
  (test '|}2| readstr "}2")
  (test '|]2| readstr "]2")
  (err/rt-test (readstr "#{1}") exn:read?)
  (err/rt-test (readstr "#[1]") exn:read?)
  (err/rt-test (readstr "#2{1}") exn:read?)
  (err/rt-test (readstr "#2[1]") exn:read?)
  (err/rt-test (readstr ",2") exn:read?)
  (err/rt-test (readstr ",@2") exn:read?)
  (err/rt-test (readstr "`2") exn:read?))

(test '(1 2 3) readstr "(2 . 1 . 3)")
(test '(1 2 3 4) readstr "(2 . 1 . 3 4)")
(test '(1 2 3 4) readstr "(2 3 . 1 . 4)")

(test 2 vector-length (readstr "#2()"))
(test 0 vector-ref (readstr "#2()") 1)
(test 2 vector-length (readstr "#000000000000000000000000000000002()"))

(err/rt-test (readstr "#2(1 2 3)") exn:read?)
(err/rt-test (readstr "#200000000000(1 2 3)") (readerrtype exn:misc:out-of-memory?))
(err/rt-test (readstr "#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111x1(1 2 3)") exn:read?)

(test #t (lambda (x) (eq? (car x) (cdr x))) (readstr "(#1=(1 2) . #0001#)"))
(test #t (lambda (x) (and (box? x) (eq? x (unbox x)))) (readstr "#0=#&#0#"))
(test #t (lambda (x) (and (vector? x) (eq? x (vector-ref x 0)) (eq? x (vector-ref x 1)))) (readstr "#0=#2(#0#)"))
(test #t (lambda (x) (and (vector? x) (eq? (vector-ref x 1) (vector-ref x 2)))) (readstr "#3(#0=(1 2) #0#)"))
(test '(1 1 1) readstr "(#0=1 #1=#0# #1#)")

;; Check that syntax, expansion, etc. preserve vector sharing
(test #t (lambda (x) (and (vector? x) (eq? (vector-ref x 0) (vector-ref x 1)))) #2((1 2)))

(define (graph-error-test readstr)
  (err/rt-test (readstr "#0#") exn:read?)
  (err/rt-test (readstr "#0=#0#") exn:read?)
  (err/rt-test (readstr "#0=#0#") exn:read?)
  (err/rt-test (readstr "(#0# #0=7)") exn:read?)
  (err/rt-test (readstr "(#0=7 #1#)") exn:read?)
  (err/rt-test (readstr "(#0=7 #0=7)") exn:read?)
  (err/rt-test (readstr "#0=") exn:read:eof?)
  (err/rt-test (readstr "#0") exn:read:eof?)
  (err/rt-test (readstr "#012345678=7") exn:read?)
  (err/rt-test (readstr "(#12345678=7 #012345678#)") exn:read?)
  (err/rt-test (readstr "#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111x1=(1 2 3)") exn:read?)
  (parameterize ([read-accept-graph #f])
    (err/rt-test (readstr "#1=1") exn:read?)
    (err/rt-test (readstr "#1#") exn:read?)))
(graph-error-tests readstr)
(graph-error-tests (lambda (s)
		     (read-syntax (open-input-string s) "string")))

;; Long symbol:
(test 'abcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefg
      readstr "abcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefgabcdefg")

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

(test 'a 'quote '\a)
(test '|\a| 'quote '\\a)
(test 'a 'quote '||||a||)
(test (string->symbol "aaa") 'quote 'aAa)
(test (string->symbol "aAa") 'quote 'A\AA)
(test (string->symbol "aAa") 'quote '|aAa|)
(test (string->symbol "aAa") 'quote 'A|A|A)

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
;; Test non-character results for getc

(define-struct special (size))

(define a-special (make-special 7))
(define b-special (make-special 19))

(define (make-p stream special-size check-pos)
  ;; The `stream' arg is a list of strings and non-strings;
  ;;  characters from the strings are returned one by one,
  ;;  and the non-strings are returns as "special" literals.
  ;; The `special-size' arg meansures the size (in char
  ;;  positions) of a non-string special literal.
  (let* ([pos 0]
	 [incpos! (lambda () (set! pos (add1 pos)))])
    (make-input-port
     ;; Read char:
     (lambda ()
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
		    [(zero? p) (incpos!)
		     (lambda (where line col pos)
		       (check-pos where line col pos)
		       (values (car s) (special-size (car s))))]
		    [else (loop (cdr s) (sub1 p))]))))))
     ;; Char ready?
     (lambda () #t)
     ;; Close proc
     (lambda () #t)
     ;; Peek proc
     #f)))

;; Simple read:
(let* ([p (make-p `("(list "
		    ,a-special
		    " "
		    ,b-special
		    "))")
		  special-size
		  (lambda (w l c p)
		    (test #f 'no-place w)
		    (test 1 'no-place l)
		    (test p 'no-place c)
		    (test #f not (memq p '(7 15)))))]
       [_ (port-count-lines! p)]
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
		  special-size
		  (lambda (w l c p)
		    (test 'dk 'dk-place w)
		    (test 8 'no-place l)
		    (test p + c 630)
		    (test #f not (memq p '(707 715)))))]
       [_ (port-count-lines! p)]
       [v (read-syntax 'dk p '(7 70 700))]
       [l (syntax->list v)]
       [v2 (syntax-object->datum v)])
  (test 'list car v2)
  (test a-special cadr v2)
  (test b-special caddr v2)
  (test 'end cadddr v2)
  
  (test 702 syntax-position (car l))
  (test 707 syntax-position (cadr l))
  (test 715 syntax-position (caddr l))
  (test 735 syntax-position (cadddr l))

  ;; Read with specials as syntax syntax already:
  (let* ([stx v]
	 [p (make-p `("(list "
		      ,stx
		      " end))")
		    (lambda (x)
		      ;; pretend it's 100 wide
		      100)
		    (lambda (w l c p)
		      (test 'dk 'dk-place w)
		    (test #f 'no-place l)
		    (test #f 'no-place c)
		    (test 7 'place p)))]
	 [v (read-syntax 'dk p)]
	 [l (syntax->list v)])
    ;; make sure syntax object is intact:
    (test stx cadr l)
    (test 108 syntax-position (caddr l))

    ;; Check that plain read performs a syntax-object->datum:
    (let* ([p (make-p `("(list "
			,stx
			" end))")
		      (lambda (x) 100)
		    (lambda (w l c p)
		      (test #f 'no-place w)
		      (test #f 'no-place l)
		      (test #f 'no-place c)
		      (test 7 'place p)))]
	   [v (read p)])
      (test `(list (list ,a-special ,b-special end) end) values v))))

;; Check that syntax read with with a list special
;;  syntaxizes the list.
(let* ([p (make-p `("(list "
		    ,(list a-special b-special)
		    " end))")
		  (lambda (x)
		    100)
		  (lambda (w l c p)
		    (test 'dk 'dk-place w)
		    (test #f 'no-place l)
		    (test #f 'no-place c)
		    (test 7 'place p)))]
       [v (read-syntax 'dk p)]
       [l (syntax->list v)])
  (test #t syntax? (cadr l))
  (test #t list? (syntax-e (cadr l)))
  (test a-special syntax-e (car (syntax-e (cadr l))))
  (test b-special syntax-e (cadr (syntax-e (cadr l))))
  (test 108 syntax-position (caddr l)))

;; Test delimitting and unsupported positions:
(test (list 1 a-special) read (make-p (list "(1" a-special ")") (lambda (x) 1) void))
(test (list 'a a-special 'b) read (make-p (list "(a" a-special "b)") (lambda (x) 1) void))
(test (list #\a a-special) read (make-p (list "(#\\a" a-special ")") (lambda (x) 1) void))
(test (list #\newline a-special) read (make-p (list "(#\\newline" a-special ")") (lambda (x) 1) void))
(test (list 5) read (make-p (list "(; \"" a-special "\n5)") (lambda (x) 1) void))
(test (list 5) read (make-p (list "(#| \"" a-special " |# 5)") (lambda (x) 1) void))
(test (list 5) read (make-p (list "(;" a-special "\n 5)") (lambda (x) 1) void))
(test 5 read (make-p (list "#| \"" a-special " |# 5") (lambda (x) 1) void))
(test 5 read (make-p (list ";" a-special "\n 5") (lambda (x) 1) void))

(err/rt-test (read (make-p (list "\"a" a-special "\"") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "\"" a-special "\"") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "\"\\" a-special "\"") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "\"\\x" a-special "\"") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "\"\\x1" a-special "\"") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "#\\" a-special "") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "#\\12" a-special "") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "#" a-special "") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "x\\" a-special "y") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "|" a-special "y|") (lambda (x) 1) void)) exn:read:non-char?)
(err/rt-test (read (make-p (list "|x" a-special "y|") (lambda (x) 1) void)) exn:read:non-char?)

;; Test read-char-or-special:
(let ([p (make-p (list "x" a-special "y") (lambda (x) 5) void)])
  (test #\x peek-char-or-special p)
  (test 0 file-position p)
  (test #\x read-char-or-special p)
  (test 1 file-position p)
  (test 'special peek-char-or-special p)
  (test 1 file-position p)
  (test a-special read-char-or-special p)
  (test 6 file-position p)
  (test #\y peek-char-or-special p)
  (test 6 file-position p)
  (test #\y read-char-or-special p)
  (test 7 file-position p))

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
