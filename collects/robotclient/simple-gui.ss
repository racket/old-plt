(module simple-gui mzscheme

  (require (lib "mred.ss" "mred")
           (lib "class.ss"))

  (define (get-new-player line)
    (let ((strnum (substring line 23 (string-length line))))
      (string->number strnum)))

  (define (read-board lineport iport)
    (let* ([onecolon (read lineport)]
	   [weirdzero (read lineport)]
	   [width (read lineport)]
	   [height (read lineport)])
      (let loop ((i 0) (acc '()))
	(cond
	 ((= i height) (list width height (apply string-append (reverse acc))))
	 (else (let* ([new-line (read-line iport)]
		      [new-line (format "~a~n"
					(substring new-line 10
						   (string-length new-line)))])
		 (loop (add1 i) (cons new-line acc))))))))

  (define (parse-intro iport)
    (let loop ((line (read-line iport))
	       (players '())
	       (board #f))
      (let* ([lineport (open-input-string line)])
	(case (read lineport)
	  ((a) (loop (read-line iport)
		     (cons (get-new-player line) players)
		     board))
	  ((turn) (if board (values line (car board) (cadr board)
				    (caddr board))
		      (let ([board (read-board lineport iport)])
			(loop (read-line iport) players board))))))))

  (define (parse-input iport)
    (let-values ([(line width height board) (parse-intro iport)])
      (let ([gui (initialize-gui width height board)])
	(let loop ((line line))
	  (unless (eof-object? line)
	    (let ([lineport (open-input-string line)])
	      (case (read lineport)
		((turn) (let* ((numcolon (read lineport))
			       (nextnum (read lineport)))
			  (case nextnum
			    ((o) (let pos-loop ((acc '()))
				   (let ((pnum (read lineport)))
				     (cond
				      ((eof-object? pnum)
				       (update-gui gui acc)
				       (loop (read-line iport)))
				      (else (let* ((x (read lineport))
						   (y (read lineport)))
					      (pos-loop
					       (cons (list pnum x y)
						     acc))))))))
			    (else (loop (read-line iport))))))
		(else (loop (read-line ip))))))))))

  (define (initialize-gui width height board)
    (initialize gui% () (board board) (width width) (height height)))

  (define (update-gui gui acc)
    (send set-robots acc))

  (define gui%
    (class object%
      
      (init-field board width height)

      (define f (instantiate frame% ("Simple Gui" #f 200 200)))
      (define c (instantiate editor-canvas% (f)))
      (define t (instantiate text% ()))
      (send c set-editor t)
      (send f show #t)

      (define (display-board b)
        (send t select-all)
        (send t delete)
        (send t insert b 1))
      
      (display-board board)
      
      
      (define/public (set-robots l)
        (let ((b (string-copy board)))
          (for-each
           (lambda (robot)
             (string-set! b (+ (* width (sub1 (caddr robot)))
                               (sub1 (cadr robot)))
                          (string-ref (number->string (car robot) 16) 0)))
           l)
          (display-board b)))
      (super-instantiate ())))

  (parse-input (current-input-port))

  )

(require simple-gui)