(unit/sig ()
  (import [wx : wx^]
	  [mred : mred^]
	  [core : mzlib:core^]
	  [pc : mzlib:print-convert^]
	  (drscheme : drscheme:export^)
	  [zodiac : drscheme:zodiac^])
  
  (define system-parameterization (current-parameterization))

  (define test-thread
    (let ([kill-old void])
      (lambda (test thunk)
	(kill-old)
	(let ([thread-desc (thread
			    (lambda ()
			      (with-parameterization system-parameterization
				(lambda ()
				  (let ([p (core:file@:normalize-path test)])
				    (printf "t>> ~a started~n" p)
				    (thunk)
				    (printf "t>> ~a finished~n" p))))))])
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
       (read-eval-print-loop)
       (printf "~nt>> REPL finished~n"))))

  (define (run-test-suite file) (test-thread file (lambda () (load/cd file))))

  (mred:set-preference-default 'drscheme:test-suite:file-name "repl-tests.ss" string?)
  (mred:set-preference-default 'drscheme:test-suite:run-interval 10 number?)

  (define (ask-test-suite)
    (let* ([drscheme-test-dir (ormap (lambda (d)
				       (let ([d (build-path d 'up "tests" "drscheme")])
					 (if (directory-exists? d)
					     d
					     #f)))
				     (current-library-collection-paths))]
	   [frame (make-object mred:frame% null "Test Suites")]
	   [panel (make-object mred:vertical-panel% frame)]
	   [top-panel (make-object mred:vertical-panel% panel)]
	   [bottom-panel (make-object mred:horizontal-panel% panel)])
      (send top-panel stretchable-in-y #f)
      (send (make-object mred:button% bottom-panel 
			 (lambda (_1 _2)
			   (send frame show #f)
			   (make-repl))
			 "REPL")
	    set-focus)
      
      (when drscheme-test-dir
	(send top-panel stretchable-in-y #t)
	(send bottom-panel stretchable-in-y #f)
	(let* ([tests
		(core:function@:quicksort
		 (core:function@:foldl 
		  (lambda (x l)
		    (if (and (file-exists? (build-path drscheme-test-dir x))
			     (>= (string-length x) 3)
			     (string=? ".ss" (substring x (- (string-length x) 3) (string-length x))))
			(cons x l)
			l))
		  null
		  (directory-list drscheme-test-dir))
		 string<=?)]
	       [lb (make-object mred:list-box% top-panel null null wx:const-single -1 -1 -1 -1 tests)])

	  ;; set values from preferences
	  (let* ([test-suite (mred:get-preference 'drscheme:test-suite:file-name)])
	    (send lb set-string-selection test-suite)
	    (send lb set-first-item test-suite)
	    (mred:test:run-interval (mred:get-preference 'drscheme:test-suite:run-interval)))

	  (send
	   (make-object mred:button% bottom-panel (lambda (_1 _2)
						    (let ([selection (send lb get-selection)])
						      (if (null? selection)
							  (wx:bell)
							  (begin
							    (send frame show #f)
							    (let ([test (list-ref tests selection)])
							      (mred:set-preference
							       'drscheme:test-suite:file-name
							       test)
							      (run-test-suite 
							       (build-path drscheme-test-dir test)))))))
			"Run Test Suite")
	   set-focus))

	(let* ([pre-times (list 0 10 50 100 500)]
	       [times (if (member (mred:test:run-interval) pre-times)
			  pre-times
			  (append pre-times (list (mred:test:run-interval))))]
	       [choice
		(make-object mred:choice%
		  top-panel
		  (lambda (choice event)
		    (let ([time (list-ref times (send event get-command-int))])
		      (mred:set-preference 'drscheme:test-suite:run-interval time)
		      (mred:test:run-interval time)))
		  "Run Interval"
		  -1 -1 -1 -1
		  (map number->string times))])
	    (send choice set-selection
		  (let loop ([l times]
			     [n 0])
		    (if (= (car l) (mred:test:run-interval))
			n
			(loop (cdr l)
			      (+ n 1)))))))
      (make-object mred:button% bottom-panel 
		   (lambda (_1 _2)
		     (send frame show #f))
		   "Cancel")
      (send frame show #t)))

  (drscheme:get/extend:extend-unit-frame%
   (lambda (super%)
     (class super% args
       (inherit button-panel)
       (sequence (apply super-init args))
       (private
	 [bitmap (make-object wx:bitmap% 
			      (if (<= (wx:display-depth) 1)
				  (build-path (collection-path "icons")
					      "bb-sm-bw.bmp")
				  (build-path (collection-path "icons")
					      "bb-small.bmp"))
			      wx:const-bitmap-type-bmp)]
	 [button (make-object
		  mred:button%
		  button-panel
		  (lambda (button evt)
		    (ask-test-suite))
		  (if (send bitmap ok?)
		      bitmap
		      "Console"))])
       (sequence
	 (send button-panel change-children
	       (lambda (l)
		 (cons button (core:function@:remq button l)))))))))
