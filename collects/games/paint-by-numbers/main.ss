(unit/sig BOARD^

  (import [GUI : GUI^]
	  [SOLVE : SOLVE^]
	  mzlib:pretty-print^
	  mred^)

  (define-struct problem (name rows cols solution))

  (include "problems.ss")

  (define semaphore (make-semaphore 0))
  (define sema-frames-open 0)
  (define sema-frame%
    (class frame% (name)
      (override
       [on-close
	(lambda ()
	  (set! sema-frames-open (- sema-frames-open 1))
	  (when (zero? sema-frames-open)
	    (semaphore-post semaphore)))])
      (sequence
	(set! sema-frames-open (+ sema-frames-open 1))
	(super-init name))))
  
  (define game-name "Paint by Numbers")

  (define frame (make-object sema-frame% game-name))

  (define menu-bar (make-object menu-bar% frame))
  (define file-menu (make-object menu% "File" menu-bar))
  (make-object menu-item% "Open..." file-menu (lambda (_1 _2) (open)) #\o)
  (make-object menu-item% "Save..." file-menu (lambda (_1 _2) (save)) #\s)
  (make-object menu-item% "Save As..." file-menu (lambda (_1 _2) (save-as)))
  (make-object menu-item% "Close" file-menu (lambda (_1 _2) (close)) #\w)
  (define edit-menu (make-object menu% "Edit" menu-bar))
  (make-object menu-item% "Undo" edit-menu (lambda (_1 _2) (send canvas undo)) #\z)
  (make-object menu-item% "Redo" edit-menu (lambda (_1 _2) (send canvas redo)) #\y)
  (define pbn-menu (make-object menu% "Nonogram" menu-bar))
  (make-object menu-item% "Solve" pbn-menu (lambda (_1 _2) (solve)) #\l)
  (make-object menu-item% "Show Mistakes" pbn-menu (lambda (_1 _2) (show-wrong)) #\h)
  (make-object menu-item% "Design a puzzle" pbn-menu (lambda (_1 _2) (editor)) #\r)

  (define top-panel (make-object horizontal-panel% frame))
  (define choice (make-object choice%
		   "Choose a Board"
		   (map problem-name problems)
		   top-panel
		   (lambda (choice evt)
		     (set-problem (list-ref problems (send choice get-selection))))))

  (define button-panel (make-object vertical-panel% top-panel))
  
  (define (solve)
    (send canvas all-unknown)
    (send canvas on-paint)
    (SOLVE:solve
     (problem-rows problem)
     (problem-cols problem)))

  (define solve-button
    (make-object button%
      "Solve"
      button-panel
      (lambda (button evt)
	(solve))))

  (define wrong-button
    (make-object button%
      "Show Mistakes"
      button-panel
      (lambda (button evt)
	(show-wrong))))
  
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
    (call-with-output-file filename
      (lambda (port)
	(pretty-print
	 (list (problem-name problem)
	       (problem-rows problem)
	       (problem-cols problem)
	       (problem-solution problem)
	       (send canvas get-grid))
	 port))
      'truncate))

  (define (save)
    (if filename
	(do-save filename)
	(save-as)))

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
    (send wrong-button enable (problem-solution prlmb))
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
	     (if (string=? "John" (problem-name problem))
		 (begin (set-problem problem)
			(send choice set-selection n))
		 (loop (+ n 1)
		       (cdr ps))))]))

  (define (set-entry i j nv)
    (send canvas set-rect i j nv)
    (send canvas paint-rect i j))

  (define (get-entry i j)
    (send canvas get-rect i j))

  (define (setup-progress max)
    (let* ([f (parameterize ([current-eventspace (make-eventspace)])
		(make-object frame% "Solver Setup Progress"))]
	   [g (make-object gauge% #f max f)]
	   [counter 0])
      (send g min-width 300)
      (send f show #t)
      (lambda ()
	(set! counter (+ 1 counter))
	(cond
	 [(= counter max)
	  (collect-garbage)
	  (send f show #f)]
	 [else (send g set-value counter)]))))

  (define (show-wrong)
    (let loop ([i (length (problem-cols problem))])
      (unless (zero? i)
	(let loop ([j (length (problem-rows problem))])
	  (unless (zero? j)
	    (let* ([m (- i 1)]
		   [n (- j 1)]
		   [board-entry (get-entry m n)]
		   [real-answer (vector-ref (vector-ref (problem-solution problem) m) n)])
	      (unless (or (eq? board-entry real-answer)
			  (eq? board-entry 'unknown)
			  (eq? real-answer 'unknown))
		(set-entry m n 'wrong)))
	    (loop (- j 1))))
	(loop (- i 1)))))

  (define (editor)
    (let* ([biggest 35]
	   [default 15]
	   [d (make-object dialog% "Size")]
	   [m (make-object message% "How big should the designer be?" d)]
	   [wp (make-object horizontal-panel% d)]
	   [wm (make-object message% "Width" wp)]
	   [gw (make-object slider% #f 1 biggest wp void default)]
	   [hp (make-object horizontal-panel% d)]
	   [hm (make-object message% "Height" hp)]
	   [gh (make-object slider% #f 1 biggest hp void default)]
	   [bp (make-object horizontal-panel% d)]
	   [cancelled? #f]
	   [cancel (make-object button% "Cancel" bp (lambda (_1 _2)
						      (set! cancelled? #t)
						      (send d show #f)))]
	   [ok (make-object button% "OK" bp (lambda (_1 _2) (send d show #f)) '(border))])

      (let ([label-width (max (send wm get-width)
			      (send hm get-width))])
	(send wm min-width label-width)
	(send hm min-width label-width))

      (send bp set-alignment 'right 'center)
      
      (send d show #t)
      (unless cancelled?
	(let ([f (make-object sema-frame% "Designer")])
	  (make-object GUI:design-paint-by-numbers-canvas% f
		       (send gw get-value)
		       (send gh get-value))
	  (send f show #t)))))

  (send frame show #t)
  (yield semaphore))