;;; repl-test.ss

(require-library "function.ss")
(require-library "file.ss")

(load "drscheme-test-util.ss")

(let-struct test (program r4rs-load-answer prepend-filename? r4rs-execute-answer r4rs-execute-location
			  mred-execute-answer mred-load-answer mred-read-test? breaking-test?)
(letrec*([test-data
	   (list

	    ;; basic tests
	    (make-test "("
		    "1.1-1.2: missing close paren"
		    #t
		    "missing close paren"
		    (vector 0 1)
		    "read: expected a ')'; started at position 1, line 1 in "
		    "read: expected a ')'; started at position 1, line 1 in "
		    #t
		    #f)
	    (make-test "."
		    "1.1-1.2: can't use `.' outside list"
		    #t
		    "can't use `.' outside list"
		    (vector 0 1)
		    "read: illegal use of \".\" at position 1 line 1 in "
		    "read: illegal use of \".\" at position 1 line 1 in "
		    #t
		    #f)
	    (make-test "(begin)"
		    "1.1-1.8: Malformed begin"
		    #t
		    "Malformed begin"
		    (vector 0 7)
		    "begin: bad syntax (empty form) in: (begin)"
		    "begin: bad syntax (empty form) in: (begin)"
		    #f
		    #f)
	    (make-test "x"
		    "1.1-1.2: reference to undefined identifier: x"
		    #t
		    "reference to undefined identifier: x"
		    (vector 0 1)
		    "reference to undefined identifier: x"
		    "reference to undefined identifier: x"
		    #f
		    #f)
	    (make-test "(raise 1)"
		    "uncaught exception: 1"
		    #f
		    "uncaught exception: 1"
		    #f
		    "uncaught exception: 1"
		    "uncaught exception: 1"
		    #f
		    #f)
	    (make-test "(raise #f)"
		    "uncaught exception: #f"
		    #f
		    "uncaught exception: #f"
		    #f
		    "uncaught exception: #f"
		    "uncaught exception: #f"
		    #f
		    #f)
	    (make-test "(values 1 2)"
		    (format "1~n2")
		    #f
		    (format "1~n2")
		    #f
		    (format "1~n2")
		    (format "1~n2")
		    #f
		    #f)
	    (make-test "(list 1 2)"
		    "(1 2)"
		    #f
		    "(1 2)"
		    #f
		    "(1 2)"
		    "(1 2)"
		    #f
		    #f)

	    ;; eval tests
	    (make-test "    (eval '(values 1 2))"
		    (format "1~n2")
		    #f
		    (format "1~n2")
		    #f
		    (format "1~n2")
		    (format "1~n2")
		    #f
		    #f)
	    (make-test "    (eval '(list 1 2))"
		    "(1 2)"
		    #f
		    "(1 2)"
		    #f
		    "(1 2)"
		    "(1 2)"
		    #f
		    #f)
	    (make-test "    (eval '(begin))"
		    "1.5-1.20: Malformed begin"
		    #t
		    "Malformed begin"
		    (vector 4 19)
		    "begin: bad syntax (empty form) in: (begin)"
		    "begin: bad syntax (empty form) in: (begin)"
		    #f
		    #f)
	    (make-test "    (eval 'x)"
		    "1.5-1.14: reference to undefined identifier: x"
		    #t
		    "reference to undefined identifier: x"
		    (vector 4 13)
		    "reference to undefined identifier: x"
		    "reference to undefined identifier: x"
		    #f
		    #f)


	    ;; error in the middle
	    (make-test "1 2 ( 3 4"
		    "1.5-1.6: missing close paren"
		    #t
		    (format "1~n2~nmissing close paren")
		    (vector 4 5)
		    (format "1~n2~nread: expected a ')'; started at position 5, line 1 in ")
		    (format "read: expected a ')'; started at position 5, line 1 in ")
		    #t
		    #f)
	    (make-test "1 2 . 3 4"
		    "1.5-1.6: can't use `.' outside list"
		    #t
		    (format "1~n2~ncan't use `.' outside list")
		    (vector 4 5)
		    (format "1~n2~nread: illegal use of \".\" at position 5 line 1 in ")
		    (format "read: illegal use of \".\" at position 5 line 1 in ")
		    #t
		    #f)
	    (make-test "1 2 x 3 4"
		    "1.5-1.6: reference to undefined identifier: x"
		    #t
		    (format "1~n2~nreference to undefined identifier: x")
		    (vector 4 5)
		    (format "1~n2~nreference to undefined identifier: x")
		    (format "reference to undefined identifier: x")
		    #f
		    #f)
	    (make-test "1 2 (raise 1) 3 4"
		    "uncaught exception: 1"
		    #f
		    (format "1~n2~nuncaught exception: 1")
		    'unlocated-error
		    (format "1~n2~nuncaught exception: 1")
		    (format "uncaught exception: 1")
		    #f
		    #f)
	    (make-test "1 2 (raise #f) 3 4"
		    "uncaught exception: #f"
		    #f
		    (format "1~n2~nuncaught exception: #f")
		    'unlocated-error
		    (format "1~n2~nuncaught exception: #f")
		    "uncaught exception: #f"
		    #f
		    #f)

	    ;; error escape handler test
	    (make-test (format "(let ([old (error-escape-handler)])~n(+ (let/ec k~n(dynamic-wind~n(lambda () (error-escape-handler (lambda () (k 5))))~n(lambda () (car))~n(lambda () (error-escape-handler old))))~n10))")
		    (format "5.12-5.17: car: expects 1 argument, given 0~n15")
		    #t
		    (format "car: expects 1 argument, given 0~n15")
		    (vector 138 143)
		    (format "car: expects 1 argument, given 0~n15")
		    (format "car: expects 1 argument, given 0~n15")
		    #f
		    #f)


	    ;; macro tests
	    (make-test "(define-macro m (lambda (x) (+ x 1))) (m 2)"
		    "3"
		    #f
		    "3"
		    #f
		    "3"
		    "3"
		    #f
		    #f)
	    (make-test "(define-macro m (lambda (x) `(+ ,x 1))) (m (+ 1 2))"
		    "4"
		    #f
		    "4"
		    #f
		    "4"
		    "4"
		    #f
		    #f)
	    (make-test "(define-macro m (car))"
		    "1.17-1.22: car: expects 1 argument, given 0"
		    #t
		    "car: expects 1 argument, given 0"
		    (vector 16 21)
		    "car: expects 1 argument, given 0"
		    "car: expects 1 argument, given 0"
		    #f
		    #f)
	    (make-test
	     (format "(define-macro m (lambda () (car)))~n(m)")
	     "1.28-1.33: car: expects 1 argument, given 0"
	     #t
	     "car: expects 1 argument, given 0"
	     (vector 27 32)
	     "car: expects 1 argument, given 0"
	     "car: expects 1 argument, given 0"
	     #f
	     #f)
	    (make-test
	     (format "(define-macro m (lambda (x) `(+ ,x 1)))~n(m #t)")
	     "2.1-2.7: +: expects type <number> as 1st argument, given: #t; other arguments were: 1"
	     #t
	     "+: expects type <number> as 1st argument, given: #t; other arguments were: 1"
	     (vector 40 46)
	     "+: expects type <number> as 1st argument, given: #t; other arguments were: 1"
	     "+: expects type <number> as 1st argument, given: #t; other arguments were: 1"
	     #f
	     #f)
	    (make-test
	     "(define-macro m 1)"
	     "1.1-1.19: Expander is not a procedure"
	     #t
	     "Expander is not a procedure"
	     (vector 0 18)
	     "define-macro: not a procedure"
	     "define-macro: not a procedure"
	     #f
	     #f)
	    (make-test
	     "(define-macro m (values (let ([x (lambda (x) x)]) x) (let ([y (lambda (x) x)]) y)))"
	     "1.17-1.83: context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
	     #t
	     "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
	     (vector 16 82)
	     "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
	     "context expected 1 value, received 2 values: #<procedure:x> #<procedure:y>"
	     #f
	     #f)
	    (make-test
	     (format "(define-macro m (lambda (x) (values x x)))~n(m 1)")
	     "1.29-1.41: context expected 1 value, received 2 values: 1 1"
	     #t
	     "context expected 1 value, received 2 values: 1 1"
	     (vector 28 40)
	     "context expected 1 value, received 2 values: 1 1"
	     "context expected 1 value, received 2 values: 1 1"
	     #f
	     #f)

	    ;; breaking tests
	    (make-test
	     (format "(let l()(l))")
	     "user break"
	     #t
	     "user break"
	     (vector 28 40)
	     "user break"
	     "user break"
	     #f
	     #t))]

	  [drscheme-frame (wait-for-drscheme-frame)]
	  [user-directory
	   (normalize-path
	    ((in-parameterization (ivar (ivar drscheme-frame interactions-edit) user-param)
				  current-directory)))]

	  [interactions-edit (ivar drscheme-frame interactions-edit)]
	  [interactions-canvas (ivar drscheme-frame interactions-canvas)]
	  [definitions-edit (ivar drscheme-frame definitions-edit)]
	  [definitions-canvas (ivar drscheme-frame definitions-canvas)]
	  [execute-button (ivar drscheme-frame execute-button)]
	  [insert-string
	   (lambda (string)
	     (let loop ([n 0])
	       (unless (= n (string-length string))
		 (let ([c (string-ref string n)])
		   (if (char=? c #\newline)
		       (mred:test:keystroke #\return)
		       (mred:test:keystroke c)))
		 (loop (+ n 1)))))]
	  [wait-for-execute (lambda () (wait-for-button execute-button))]
	  [get-int-pos (lambda () (get-text-pos interactions-edit))]


	  [do-execute 
	   (lambda ()
	     (push-button-and-wait execute-button))]

	  [tmp-load-filename
	   (normalize-path (build-path (current-load-relative-directory) "repl-test-tmp.ss"))]

	  ;; given a filename "foo", we perform two operations on the contents 
	  ;; of the file "foo.ss".  First, we insert its contents into the REPL
	  ;; directly, and second, we use the load command.  We compare the
	  ;; the results of these operations against expected results.
	  
	  [run-test
	   (lambda (execute-text-start escape mred?)
	     (lambda (in-vector)
	       (let* ([program (test-program in-vector)]
		      [pre-answer-load (test-r4rs-load-answer in-vector)]
		      [prepend-filename? (test-prepend-filename? in-vector)]
		      [answer-load (if prepend-filename?
				       (string-append tmp-load-filename ": " pre-answer-load)
				       pre-answer-load)]
		      [answer-execute (test-r4rs-execute-answer in-vector)]
		      [execute-location (test-r4rs-execute-location in-vector)]
		      [mred-execute-answer (test-mred-execute-answer in-vector)]
		      [mred-load-answer (test-mred-load-answer in-vector)]
		      [mred-read-test? (test-mred-read-test? in-vector)]
		      [breaking-test? (test-breaking-test? in-vector)])

		 (clear-definitions drscheme-frame)
		 ; load contents of test-file into the REPL, recording
		 ; the start and end positions of the text
		 
		 (insert-string program)
		 (do-execute)
		 (let* ([execute-text-end (- (get-int-pos) 1)] ;; subtract one to skip last newline
			[received-execute
			 (send interactions-edit get-text 
			       execute-text-start execute-text-end)])

		   ; check focus and selection for execute test
		   (unless mred?
		     (cond
		      [(eq? execute-location 'unlocated-error) 
		       (unless (send interactions-canvas is-focus-on?)
			 (printf "FAILED execute test for ~s~n  expected interactions to have the focus~n"
				 program))]
		      [(and execute-location (send definitions-canvas is-focus-on?))
		       (unless (and (= (send definitions-edit get-start-position) (vector-ref execute-location 0))
				    (= (send definitions-edit get-end-position) (vector-ref execute-location 1)))
			 (printf "FAILED execute test for ~s~n  start/end position are ~a ~a~n  expected ~a ~a~n"
				 program
				 (send definitions-edit get-start-position)
				 (send definitions-edit get-end-position)
				 (vector-ref execute-location 0)
				 (vector-ref execute-location 1)))]
		      [execute-location
		       (printf "FAILED execute test for ~s~n  expected definitions canvas to have the focus~n"
			       program)]
		      [(not (send interactions-canvas is-focus-on?))
		       (printf "FAILED execute test for ~s~n  expected interactions to have the focus~n"
			       program)]
		      [else (void)]))

		   ; check text for execute test
		   (let ([expected
			  (if mred?
			      (if mred-read-test?
				  (string-append mred-execute-answer "USERPORT")
				  mred-execute-answer)
			      answer-execute)])
		     (unless (string=? received-execute expected)
		       (printf "FAILED execute test for ~s~n  expected: ~s~n       got: ~s~n"
			       program expected received-execute)))

		   (mred:test:new-window interactions-canvas)
		   
		   ; construct the load file

		   (call-with-output-file tmp-load-filename
		     (lambda (port) (display program port))
		     'truncate)

		   ; stuff the load command into the REPL 
		   
		   (for-each mred:test:keystroke
			     (string->list (format "(load ~s)"
						   (find-relative-path
						    user-directory
						    tmp-load-filename))))
		   
		   ; record current text position, then stuff a CR into the REPL
		   
		   (let ([load-text-start (+ 1 (send interactions-edit last-position))])
		     
		     (mred:test:keystroke #\return)
		     
		     (wait-for-execute)
		     
		     (let* ([load-text-end (- (get-int-pos) 1)] ;; subtract one to eliminate newline
			    [received-load 
			     (send interactions-edit get-text 
				   load-text-start load-text-end)])
		       
		       ; check load text 
		       (let ([expected
			      (if mred?
				  (if mred-read-test?
				      (string-append mred-load-answer
						     tmp-load-filename)
				      mred-load-answer)
				  answer-load)])
			 (unless (string=? received-load expected)
			   (printf "FAILED load test for ~s~n  expected: ~s~n       got: ~s~n"
				   program expected received-load)))
		       
		       ; check for edit-sequence
		       (when (repl-in-edit-sequence?)
			 (printf "FAILED: repl in edit-sequence")
			 (escape))))))))]

	  [run-test-in-language-level
	   (lambda (mred?)
	     (let ([level (if mred? "MrEd" "R4RS+")])
	       (printf "running ~a tests~n" level)
	       (set-language-level! level drscheme-frame)
	       (mred:test:new-window definitions-canvas)
	       (mred:test:menu-select "Edit" "Select All")
	       (mred:test:menu-select "Edit" (if (eq? wx:platform 'macintosh)
						 "Clear"
						 "Delete"))
	       (do-execute)
	       (let/ec escape (for-each (run-test (get-int-pos) escape mred?) test-data))))])

  (run-test-in-language-level #t)
  (run-test-in-language-level #f)))
