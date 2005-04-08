
(load-relative "loadtest.ss")

(SECTION 'READTABLE)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(arity-test make-readtable 1 -1)
(arity-test readtable? 1 1)
(arity-test readtable-mapping 2 2)

(err/rt-test (make-readtable 5))
(err/rt-test (make-readtable #f 5))
(err/rt-test (make-readtable #f #\a))
(err/rt-test (make-readtable #f #\a 5))
(err/rt-test (make-readtable #f #\a #\b))
(err/rt-test (make-readtable #f #\a 'terkminating-macro))

(test #f current-readtable)
(test #t readtable? (make-readtable #f))
(test #t readtable? (make-readtable (make-readtable #f)))

(let ([plain-dollar
       (case-lambda
	[(ch port)
	 (test #t ormap (lambda (x) (char=? ch x)) '(#\$ #\&))
	 `dollar]
	[(ch port src line col pos)
	 (test #t ormap (lambda (x) (char=? ch x)) '(#\$ #\&))
	 `dollar])]
      [plain-percent
       (case-lambda
	[(ch port)
	 (test #\% values ch)
	 `(percent ,(read/recursive port))]
	[(ch port src line col pos)
	 (test #\% values ch)
	 `(percent ,(read-syntax/recursive src port))])]
      [hash-dollar
       (case-lambda
	[(ch port)
	 (test #\$ values ch)
	 `(dollar . ,(read/recursive port))]
	[(ch port src line col pos)
	 (test #\$ values ch)
	 `(dollar . ,(read-syntax/recursive src port))])]
      [comment3
       (case-lambda
	[(ch port src line col pos)
	 (test #\_ values ch)
	 (read-char port) (read-char port) (read-char port)
	 (make-special-comment #f)])])
  (let ([t (make-readtable #f 
			   #\$ 'terminating-macro plain-dollar
			   #\% 'non-terminating-macro plain-percent
			   #\^ #\| #f
			   #\< #\( #f
			   #\= #\\ #f
			   #\~ #\space #f
			   #\_ 'terminating-macro comment3
			   #\$ 'dispatch-macro  hash-dollar)])
    (test-values '(#\a #f #f) (lambda () (readtable-mapping t #\a)))
    (test-values '(#\| #f #f) (lambda () (readtable-mapping t #\^)))
    (test-values '(#\( #f #f) (lambda () (readtable-mapping t #\<)))
    (test-values '(#\\ #f #f) (lambda () (readtable-mapping t #\=)))
    (test-values '(#\space #f #f) (lambda () (readtable-mapping t #\~)))
    (test-values (list 'terminating-macro plain-dollar hash-dollar) (lambda () (readtable-mapping t #\$)))
    (test-values (list 'terminating-macro comment3 #f) (lambda () (readtable-mapping t #\_)))
    (test-values (list 'non-terminating-macro plain-percent #f) (lambda () (readtable-mapping t #\%)))
    (let ([t2 (make-readtable t
			      #\& #\$ t
			      #\a #\a t
			      #\^ #\^ #f)])
      (test-values '(#\a #f #f) (lambda () (readtable-mapping t2 #\a)))
      (test-values '(#\^ #f #f) (lambda () (readtable-mapping t2 #\^)))
      (test-values '(#\space #f #f) (lambda () (readtable-mapping t #\~)))
      (test-values (list 'terminating-macro plain-dollar #f) (lambda () (readtable-mapping t2 #\&)))

      (let ([test-read
	     (lambda (s l)
	       (define (go read)
		 (let* ([o (open-input-string s)])
		   (let loop ()
		     (let ([v (read o)])
		       (if (eof-object? v)
			   null
			   (cons v (loop)))))))
	       (test l (lambda (a b) (go read)) 'read s)
	       (test l (lambda (a b) (map syntax-object->datum 
					  (go (lambda (p) (read-syntax 'string p)))))
		     'read-syntax s))])
	
	(test-read "a$%_^b" '(a$%_^b))

	(let ([try-table
	       (lambda (t old-caret?)
		 (parameterize ([current-readtable t])
		   (test-read "a$b" '(a dollar b))
		   (when old-caret?
		     (test-read "a&b" '(a dollar b)))
		   (test-read "a #$ b" '(a (dollar . b)))
		   (test-read "(#1=a #$ #1#)" '((a (dollar . a))))
		   (test-read "(#1=a #$ (#1#))" '((a (dollar a))))
		   (test-read "a%b" '(a%b))
		   (test-read "a % b" '(a (percent b)))
		   (test-read "(#1=a % #1#)" '((a (percent a))))
		   (test-read "(#1=a % (#1#))" '((a (percent (a)))))
		   (test-read "a _xxx b" '(a b))
		   (test-read "(a _xxx b)" '((a b)))
		   (test-read "(a _xxx . b)" '((a . b)))
		   (test-read "(a . _xxx b)" '((a . b)))
		   (if old-caret?
		       (test-read "(a ^_xxx^ b)" '((a ^ ^ b)))
		       (test-read "(a ^_xxx^ b)" '((a _xxx b))))
		   (test-read "(a =_xxx b)" '((a _xxx b)))
		   (test-read "<a xxx b)" '((a xxx b)))
		   (test-read "<a~xxx~b)" '((a xxx b)))))])
	  (try-table t #f)
	  (try-table t2 #t))

	(let ([try-as-plain (lambda (ch)
			      (parameterize ([current-readtable (make-readtable #f
										ch #\a #f)])
				(let ([s1 (format "a~ab" ch)]
				      [s2 (format "~aab~a" ch ch)])
				  (test-read s1 (list (string->symbol s1)))
				  (test-read s2 (list (string->symbol s2)))
				  (let ([blank (if (char=? ch #\space)
						   #\newline
						   #\space)])
				    (test-read (format "a~a~a~ab" blank ch blank) 
					       (list 'a (string->symbol (string ch)) 'b))))))])
	  (for-each try-as-plain (string->list "()[]{}|\\ \r\n\t\v',\"#")))

	(void)))))

      
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
