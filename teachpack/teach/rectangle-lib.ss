(reference-file "big-draw-lib.ss")
(reference-file "error-lib.ss")

(define-signature rectangleS (show))

(define rectangleU
  (unit/sig rectangleS (import errorS bigDrawS plt:userspace^)

    ;; show : rectangle -> #t
    (define (show rect)
      (check-arg 'show
	(and (list? rect) (andmap (lambda (l) (and (list? l) (andmap rgb? l))) rect))
	"rectangle (list of list of colors)" "" rect)

      (clear-all)

      (let ((x 0) (y 0))
	(for-each (lambda (line)
		    (for-each (lambda (color) 
				(draw-square x y color)
				(set! x (+ x LENGTH-SQUARE)))
			      line)
		    (set! x 0)
		    (set! y (+ y LENGTH-SQUARE)))
		  rect)
	#t))
    
    ;; could be done by students -- after they learn about accumulators
    (define (show2 rect)
      (let OL ((rect rect) (y 0))
	(cond
	  ((null? rect) #t)
	  (else (let IL ((line (first rect)) (x 0))
		  (cond
		    ((null? line) (void))
		    (else (and (draw-square x y (first line))
			       (IL (rest line) (+ x LENGTH-SQUARE))))))
		(OL (rest rect) (+ y LENGTH-SQUARE))))))
    
    ;; draw-square : number number color -> #t
    (define (draw-square y x c)
      (draw-solid-rect (make-posn y x) LENGTH-SQUARE LENGTH-SQUARE c))
    
    (define LENGTH-SQUARE 10)))

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    [ERR   : errorS (errorU)]
    [DRAW  : bigDrawS  (bigDrawU ERR PLT)]
    [RECT  : rectangleS (rectangleU ERR DRAW PLT)])
  (export (open (DRAW : drawS)) (open RECT)))
