(unit/sig ()
  (import [wx : wx^]
	  [mred : mred^]
	  mzlib:core^
	  mzlib:print-convert^
	  (drscheme : drscheme:export^)
	  drscheme:zodiac^)
  
  (define rep-thread #f)
  
  (define (make-repl)
    (if (and rep-thread
	     (thread-running? rep-thread))
	(begin 
	  (kill-thread rep-thread)
	  (set! rep-thread #f)
	  (printf "REPL killed~n"))
	(set! rep-thread 
	      (thread 
	       (lambda ()
		 (let ([startup "~/.mzschemerc"])
		   (when (file-exists? startup)
		     (load startup)))
		 (read-eval-print-loop)
		 (printf "~nREPL finished~n"))))))

  (define (run-test-suite file) (thread (lambda () (load/cd file))))

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
      (when drscheme-test-dir
	(send top-panel stretchable-in-y #t)
	(send bottom-panel stretchable-in-y #f)
	(let* ([tests
		(function@:quicksort
		 (function@:foldl 
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
	  (make-object mred:button% bottom-panel (lambda (_1 _2)
						   (let ([selection (send lb get-selection)])
						     (if (null? selection)
							 (wx:bell)
							 (begin
							   (send frame show #f)
							   (run-test-suite 
							    (build-path drscheme-test-dir 
									(list-ref tests selection)))))))
		       "Run Test Suite"))
	(let ([times (list 0 10 50 100 500)])
	  (unless (member (mred:test:run-interval) times)
	    (set! times (append times (list (mred:test:run-interval)))))
	  (let ([choice
		 (make-object mred:choice%
			      top-panel
			      (lambda (choice event) (mred:test:run-interval (list-ref times (send event get-command-int))))
			      "Run Interval"
			      -1 -1 -1 -1
			      (map number->string times))])
	    (send choice set-selection
		  (let loop ([l times]
			     [n 0])
		    (if (= (car l) (mred:test:run-interval))
			n
			(loop (cdr l)
			      (+ n 1))))))))
		       
      (make-object mred:button% bottom-panel 
		   (lambda (_1 _2)
		     (send frame show #f)
		     (make-repl))
		   "REPL")
      (make-object mred:button% bottom-panel 
		   (lambda (_1 _2) (send frame show #f))
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
		 (cons button (function@:remq button l)))))))))
