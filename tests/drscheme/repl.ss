(require-library "function.ss")
(load-relative "util.ss")

(wait-for-drscheme-frame)
(letrec ([test-files 
	  (let ([dir (build-path (current-load-relative-directory) "repl-tests")]
		[get-num
		 (lambda (s)
		   (let ([ans (if (char=? (string-ref s 1) #\.)
				  (string->number (substring s 0 1))
				  (string->number (substring s 0 2)))])
		     (or ans
			 0)))])
	    (foldl (lambda (f sofar) 
		     (if (and (not (string=? (substring f 0 1) "."))
			      (string=? (substring f (- (string-length f) 2)
						   (string-length f))
					"ss"))
			 (cons (build-path dir
					   (substring 
					    f 0 
					    (- (string-length f) 2)))
			       sofar)
			 sofar))
		   null
		   (quicksort 
		    (directory-list dir)
		    (lambda (x y)
		      (<= (get-num y) (get-num x))))))]
	 [drscheme-frame (mred:test:get-active-frame)]
	 [interactions-edit (ivar drscheme-frame interactions-edit)]
	 [interactions-canvas (ivar drscheme-frame interactions-canvas)]
	 [definitions-edit (ivar drscheme-frame definitions-edit)]
	 [definitions-canvas (ivar drscheme-frame definitions-canvas)]
	 [execute-button (ivar drscheme-frame execute-button)]
	 [get-answer
	  (lambda (filename)
	    (call-with-input-file filename
	      (lambda (port)
		(apply string
		       (let loop ()
			 (let ([c (read-char port)])
			   (if (eof-object? c)
			       null
			       (cons c (loop)))))))))]
	 [insert-file
	  (lambda (filename)
	    (call-with-input-file filename
	      (lambda (port)
		(let loop ()
		  (let ([c (read-char port)])
		    (unless (eof-object? c)
		      (if (char=? c #\newline)
			  (mred:test:keystroke #\return)
			  (mred:test:keystroke c))
		      (loop)))))))]
	 [wait-for-execute
	  (rec loop
	       (lambda ()
		 (unless (send execute-button is-enabled?)
		   (sleep 1/2)
		   (loop))))]
	 [get-int-pos
	  (lambda ()
	    (let* ([last-pos (send interactions-edit last-position)]
		   [last-line (send interactions-edit position-line last-pos)]
		   [line-beginning (send interactions-edit line-start-position last-line)])
	      line-beginning))]
	 [do-execute
	  (lambda ()
	    (mred:test:button-push execute-button)
	    (wait-for-execute))]
	 [clear-definitions
	  (lambda ()
	    (mred:test:new-window definitions-canvas)
	    (mred:test:menu-select "Edit" "Select All")
	    (mred:test:menu-select "Edit" (if (eq? wx:platform 'macintosh)
					      "Clear"
					      "Delete")))]
	 [run-test
	  (lambda (file)
	    (let ([answer-load (get-answer (string-append file "load"))]
		  [answer-execute (get-answer (string-append file "execute"))]
		  [test-file (string-append file "ss")])
	      (clear-definitions)
	      (do-execute)
	      (mred:test:new-window definitions-canvas)
	      (let ([first-start (get-int-pos)])
		(insert-file test-file)
		(do-execute)
		(let ([first-end (get-int-pos)])
		  (mred:test:new-window interactions-canvas)
		  (for-each (lambda (c) (mred:test:keystroke c))
			    (string->list (format "(load ~s)" test-file)))
		  (let ([second-start (+ 1 (send interactions-edit last-position))])
		    (mred:test:keystroke #\return)
		    (wait-for-execute)
		    (let* ([second-end (get-int-pos)]
			   [received-execute
			    (send interactions-edit get-text 
				  first-start first-end)]
			   [received-load 
			    (send interactions-edit get-text 
				  second-start second-end)])
		      (unless (string=? received-execute answer-execute)
			(printf "FAILED execute ~a~n" file))
		      (unless (string=? received-load answer-load)
			(printf "FAILED load    ~a~n" file))))))))]
	 
	 [test-text-boxes
	  (lambda ()
	    (clear-definitions)
	    (mred:test:menu-select "Edit" "Insert Text Box")
	    (foreach mred:test:keystroke (string->list ";; f : 'a -> 'b"))
	    (mred:test:keystroke #\return)
	    (mred:test:new-window definitions-canvas)
	    (do-execute))])
  (printf "starting repl test~n")
  ;(test-text-boxes)
  (for-each run-test test-files)
  (printf "finished repl test~n"))
