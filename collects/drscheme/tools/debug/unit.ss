(unit/sig ()
  (import [mred : mred-interfaces^]
	  [core : mzlib:core^]
	  [fw : framework^]
	  [pc : mzlib:print-convert^]
	  (drscheme : drscheme:export^)
	  [zodiac : drscheme:zodiac^])
  
  (define test-thread
    (let ([kill-old void])
      (lambda (test thunk)
	(kill-old)
	(let ([thread-desc (thread
			    (lambda ()
			      (printf "t>> ~a started~n" test)
			      (thunk)
			      (printf "t>> ~a finished~n" test)))])
	  (set! kill-old
		(lambda ()
		  (when (thread-running? thread-desc)
		    (kill-thread thread-desc)
		    (printf "t>> killed ~a~n" test))))))))
  
  (define (make-repl)
    (test-thread
     "REPL"
     (lambda ()
       (let ([startup "~/.mzschemerc"])
	 (when (file-exists? startup)
	   (load startup)))
       (read-eval-print-loop))))

  (define (run-test-suite file)
    (test-thread
     file
     (lambda ()
       ((load (build-path (collection-path "tests" "drscheme")
			  "run-test.ss"))
	file))))

  (fw:preferences:set-default 'drscheme:test-suite:file-name "repl-tests.ss" string?)
  (fw:preferences:set-default 'drscheme:test-suite:run-interval 10 number?)

  (define current-test-suite-frame #f)

  (define (ask-test-suite)
    (if current-test-suite-frame
	(send current-test-suite-frame show #t)
	(let* ([frame% (class mred:frame% (name)
			 (override
			  [on-close
			   (lambda ()
			     (set! current-test-suite-frame #f))])
			 (sequence
			   (super-init name)))]
	       [drscheme-test-dir (collection-path "tests" "drscheme")]
	       [frame (make-object frame% "Test Suites")]
	       [panel (make-object mred:vertical-panel% frame)]
	       [top-panel (make-object mred:vertical-panel% panel)]
	       [bottom-panel (make-object mred:horizontal-panel% panel)])
	  (send top-panel stretchable-height #f)
	  (send (make-object mred:button%
		  "REPL" 
		  bottom-panel
		  (lambda (_1 _2)
		    (send frame show #f)
		    (make-repl)))
		focus)
	  
	  (when drscheme-test-dir
	    (send top-panel stretchable-height #t)
	    (send bottom-panel stretchable-height #f)
	    (let* ([tests
		    (core:function:quicksort
		     (map 
		      symbol->string
		      (call-with-input-file (build-path drscheme-test-dir "README") (core:function:compose eval read)))
		     string<=?)]
		   [lb (make-object mred:list-box%
			 #f
			 tests
			 top-panel
			 void)])

	      ;; set values from preferences
	      (let* ([test-suite (fw:preferences:get 'drscheme:test-suite:file-name)]
		     [num (send lb find-string test-suite)])
		(when num
		  (send lb set-string-selection test-suite)
		  (send lb set-first-visible-item num)
		  (fw:test:run-interval (fw:preferences:get 'drscheme:test-suite:run-interval))))

	      (send
	       (make-object mred:button%
		 "Run Test Suite"
		 bottom-panel
		 (lambda (_1 _2)
		   (let ([selection (send lb get-selection)])
		     (if (null? selection)
			 (mred:bell)
			 (begin
			   (send frame show #f)
			   (let ([test (list-ref tests selection)])
			     (fw:preferences:set
			      'drscheme:test-suite:file-name
			      test)
			     (run-test-suite
			      test)))))))
	       focus))

	    (let* ([pre-times (list 0 10 50 100 500)]
		   [times (if (member (fw:test:run-interval) pre-times)
			      pre-times
			      (append pre-times (list (fw:test:run-interval))))]
		   [choice
		    (make-object mred:choice%
		      "Run Interval"
		      (map number->string times)
		      top-panel
		      (lambda (choice event)
			(let ([time (list-ref times (send choice get-selection))])
			  (fw:preferences:set 'drscheme:test-suite:run-interval time)
			  (fw:test:run-interval time))))])
	      (send choice set-selection
		    (let loop ([l times]
			       [n 0])
		      (if (= (car l) (fw:test:run-interval))
			  n
			  (loop (cdr l)
				(+ n 1)))))))
	  (make-object mred:button%
	    "Cancel" 
	    bottom-panel
	    (lambda (_1 _2)
	      (send frame show #f)))
	  (send frame show #t)
	  (set! current-test-suite-frame frame))))

  (drscheme:get/extend:extend-unit-frame%
   (lambda (super%)
     (class super% args
       (inherit button-panel)
       (sequence (apply super-init args))
       (private
	 [bitmap (make-object mred:bitmap% 
		   (if (<= (mred:get-display-depth) 1)
		       (build-path (collection-path "icons") "bb-sm-bw.bmp")
		       (build-path (collection-path "icons") "bb-small.bmp"))
		   'bmp)]
	 [button (make-object
		  mred:button%
		  (if (send bitmap ok?)
		      bitmap
		      "Console")
		  button-panel
		  (lambda (button evt)
		    (ask-test-suite)))])
       (sequence
	 (send button-panel change-children
	       (lambda (l)
		 (cons button (core:function:remq button l)))))))))
