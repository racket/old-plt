(unit/sig MAIN^

  (import [GUI : GUI^]
	  [SOLVE : SOLVE^]
	  mzlib:pretty-print^
	  mred^)

  (define-struct problem (name rows cols solution))

  (include "problems.ss")

  (define semaphore (make-semaphore 0))
  (define sema-frame%
    (class frame% (name)
      (override
       [on-close
	(lambda ()
	  (semaphore-post semaphore))])
      (sequence (super-init name))))
  
  (define game-name "Paint by Numbers")

  (define frame (make-object sema-frame% game-name))

  (define menu-bar (make-object menu-bar% frame))
  (define file-menu (make-object menu% "File" menu-bar))
  (make-object menu-item% "Open..." file-menu (lambda (_1 _2) (open)) #\o)
  (define save-item (make-object menu-item% "Save..." file-menu (lambda (_1 _2) (save)) #\s))
  (make-object menu-item% "Save As..." file-menu (lambda (_1 _2) (save-as)))
  (make-object menu-item% "Close" file-menu (lambda (_1 _2) (close)) #\c)
  (define edit-menu (make-object menu% "Edit" menu-bar))
  (make-object menu-item% "Undo" edit-menu (lambda (_1 _2) (send canvas undo)) #\z)
  (make-object menu-item% "Redo" edit-menu (lambda (_1 _2) (send canvas redo)) #\y)

  (define top-panel (make-object horizontal-panel% frame))
  (define choice (make-object choice%
		   "Choose a Board"
		   (map problem-name problems)
		   top-panel
		   (lambda (choice evt)
		     (set-problem (list-ref problems (send choice get-selection))))))
  (define solve-button
    (make-object button%
      "Solve"
      top-panel
      (lambda (button evt) (SOLVE:solve))))
  
  (define canvas #f)
  (define problem #f)
  
  (define (close)
    (send frame show #f)
    (semaphore-post semaphore))

  (define filename #f)
  (define (update-filename new-name)
    (set! filename new-name)
    (let* ([short-name (if new-name
			   (let-values ([(_1 name _2) (split-path new-name)])
			     name)
			   #f)]
	   [new-label (if short-name
			  (format "~a - ~a" short-name game-name)
			  game-name)])
      (unless (string=? new-label (send frame get-label))
	(send frame set-label new-label))))

  (define (do-save filename)
    (update-filename filename)
    (send save-item enable #t)
    (call-with-output-file filename
      (lambda (port)
	(pretty-print
	 (list (problem-name problem)
	       (problem-cols problem)
	       (problem-rows problem)
	       (problem-solution problem)
	       (send canvas get-grid))
	 port))
      'truncate))
  (define (save)
    (when filename
      (do-save filename)))

  (define (save-as)
    (let ([filename (put-file)])
      (when filename
	(do-save filename))))

  (define (open)
    (let ([filename (get-file)])
      (when filename
	(let* ([state (call-with-input-file filename read)]
	       [name (car state)])
	  (set-problem (make-problem name (cadr state) (caddr state) (cadddr state)))
	  (send choice set-string-selection name)
	  (send canvas set-grid (car (cddddr state)))
	  (send canvas on-paint)
	  (update-filename filename)))))
	    
  (define (set-problem prlmb)
    (update-filename #f)
    (send save-item enable #f)
    (send frame change-children (lambda (x) (list top-panel)))
    (send frame stretchable-width #f)
    (send frame stretchable-height #f)
    (send frame stretchable-width #t)
    (send frame stretchable-height #t)
    (let ([rows (problem-rows prlmb)]
	  [cols (problem-cols prlmb)])
      (set! problem prlmb)
      (make-object message%
	(format "The board is ~a cells wide and ~a cells tall" (length cols) (length rows))
	frame)
      (set! canvas (make-object GUI:paint-by-numbers-canvas% frame rows cols))))

  (let loop ([n 0]
	     [ps problems])
    (cond
     [(null? ps) (set-problem (car problems))
		 (send choice set-selection 0)]
     [else (let ([problem (car ps)])
	     (if (string=? "First" (problem-name problem))
		 (begin (set-problem problem)
			(send choice set-selection n))
		 (loop (+ n 1)
		       (cdr ps))))]))

  (send frame show #t)
  (yield semaphore))