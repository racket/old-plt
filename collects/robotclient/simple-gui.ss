(module simple-gui mzscheme

  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "board.ss")

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
    (send gui set-robots acc 0 0 0 null))

  (define gui%
    (class object%
      
      ;; board: string list
      (init-field board width height)

      (define f (instantiate frame% ("Simple Gui" #f 800 800)))
      (define q (make-object panel:horizontal-dragable% f))
      (define p (make-object panel:vertical-dragable% q))
      (define pack-text (instantiate text% ()))
      (send (instantiate editor-canvas% (q)) set-editor pack-text)
      (define map-text (instantiate text% ()))
      (send (instantiate editor-canvas% (p)) set-editor map-text)
      (define log-text (instantiate text% ()))
      (send (instantiate editor-canvas% (p)) set-editor log-text)
      (send f show #t)
      
      (define (display-board b money score packages-held)
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
          (send map-text set-position (send map-text get-end-position) 'same)
          (send map-text insert (format "money: ~a~n" money))
          (send map-text insert (format "score: ~a~n" score))
          (send map-text end-edit-sequence)
          (send pack-text begin-edit-sequence)
          (send pack-text select-all)
          (send pack-text delete)
          (for-each (lambda (p)
                      (send pack-text insert
                            (format "holding package: ~a~n" (package->string p))))
                    packages-held)
          (send pack-text end-edit-sequence)))

      (define/public (change-board x y char)
        (string-set! (list-ref board (sub1 y)) (sub1 x) char))
      
      (define/public (end) (send f show #t))
      
      (define/public (log text)
        (send log-text insert text)
        (send log-text insert #\newline))
      
      (define/public (set-robots l id money score packages-held)
        (sleep/yield .05)
        (let ((b (list->vector (map string-copy board))))
          (for-each
           (lambda (robot)
             (string-set! (vector-ref b (sub1 (caddr robot)))
                          (sub1 (cadr robot))
                          (cond
                            ((= (car robot) id) #\u)
                            (else #\r))))
                          
           l)
          (display-board (reverse (vector->list b)) money score packages-held)))
      (super-instantiate ())))

  )

