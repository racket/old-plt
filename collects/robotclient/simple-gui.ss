(module simple-gui mzscheme

  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "client-parameters.ss")

  (provide gui% parse-input)
  
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
	 ((= i height) (list width height (reverse acc)))
	 (else (let* ([new-line (read-line iport)]
		      [new-line (substring new-line 10
                                           (string-length new-line))])
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
	  (if (eof-object? line)
              (send gui end)
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
                  (else (loop (read-line iport))))))))))

  (define (initialize-gui width height board)
    (instantiate gui% () (board board) (width width) (height height)))

  (define (update-gui gui acc)
    (send gui set-robots acc))

  (define gui%
    (class object%
      
      (init-field board width height)

      (define f (instantiate frame% ("Simple Gui" #f 400 400)))
      (define p (make-object panel:vertical-dragable% f))
      (define map-text (instantiate text% ()))
      (send (instantiate editor-canvas% (p)) set-editor map-text)
      (define log-text (instantiate text% ()))
      (send (instantiate editor-canvas% (p)) set-editor log-text)
      (send f show #t)

      (define (display-board b)
        (let* ((snips (map (lambda (b) (make-object string-snip% b)) b)))
          (send map-text begin-edit-sequence)
          (send map-text select-all)
          (send map-text delete)
          (for-each
           (lambda (snip)
             (send map-text insert snip)
             (send map-text insert #\newline))
           snips)
          (send map-text select-all)
          (let ((d (make-object style-delta%)))
            (send d set-face "-misc-fixed")
            (send map-text change-style d))
          (send map-text set-position 0 'same)
          (send map-text end-edit-sequence)))
      
      (define/public (end) (send f show #t))
      
      (define/public (set-robots l)
        (sleep/yield .05)
        (let ((b (list->vector (map string-copy board))))
          (for-each
           (lambda (robot)
             (string-set! (vector-ref b (sub1 (caddr robot)))
                          (sub1 (cadr robot))
                          (cond
                            ((= (car robot) (player-id)) #\u)
                            (else #\r))))
                          
           l)
          (display-board (reverse (vector->list b)))))
      (super-instantiate ())))

  )

