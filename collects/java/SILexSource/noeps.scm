; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

; Fonction "merge" qui elimine les repetitions
(define noeps-merge-1
  (lambda (l1 l2)
    (cond ((null? l1)
	   l2)
	  ((null? l2)
	   l1)
	  (else
	   (let ((t1 (car l1))
		 (t2 (car l2)))
	     (cond ((< t1 t2)
		    (cons t1 (noeps-merge-1 (cdr l1) l2)))
		   ((= t1 t2)
		    (cons t1 (noeps-merge-1 (cdr l1) (cdr l2))))
		   (else
		    (cons t2 (noeps-merge-1 l1 (cdr l2))))))))))

; Fabrication des voisinages externes
(define noeps-mkvois
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (arcs (make-vector nbnodes '())))
      (let loop1 ((n 0))
	(if (< n nbnodes)
	    (begin
	      (let loop2 ((trans (vector-ref trans-v n)) (ends '()))
		(if (null? trans)
		    (vector-set! arcs n ends)
		    (let* ((tran (car trans))
			   (class (car tran))
			   (end (cdr tran)))
		      (loop2 (cdr trans) (if (eq? class 'eps)
					     (noeps-merge-1 ends (list end))
					     ends)))))
	      (loop1 (+ n 1)))))
      arcs)))

; Fabrication des valeurs initiales
(define noeps-mkinit
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (init (make-vector nbnodes)))
      (let loop ((n 0))
	(if (< n nbnodes)
	    (begin
	      (vector-set! init n (list n))
	      (loop (+ n 1)))))
      init)))

; Traduction d'une liste d'arcs
(define noeps-trad-arcs
  (lambda (trans dict)
    (let loop ((trans trans))
      (if (null? trans)
	  '()
	  (let* ((tran (car trans))
		 (class (car tran))
		 (end (cdr tran)))
	    (if (eq? class 'eps)
		(loop (cdr trans))
		(let* ((new-end (vector-ref dict end))
		       (new-tran (cons class new-end)))
		  (cons new-tran (loop (cdr trans))))))))))

; Elimination des transitions eps
(define noeps
  (lambda (nl-start no-nl-start arcs acc)
    (let* ((digraph-arcs (noeps-mkvois arcs))
	   (digraph-init (noeps-mkinit arcs))
	   (dict (digraph digraph-arcs digraph-init noeps-merge-1))
	   (new-nl-start (vector-ref dict nl-start))
	   (new-no-nl-start (vector-ref dict no-nl-start)))
      (let loop ((i (- (vector-length arcs) 1)))
	(if (>= i 0)
	    (begin
	      (vector-set! arcs i (noeps-trad-arcs (vector-ref arcs i) dict))
	      (loop (- i 1)))))
      (list new-nl-start new-no-nl-start arcs acc))))
