; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

; Preparer les arcs pour digraph
(define sweep-mkarcs
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
	   (arcs-v (make-vector nbnodes '())))
      (let loop1 ((n 0))
	(if (< n nbnodes)
	    (let loop2 ((trans (vector-ref trans-v n)) (arcs '()))
	      (if (null? trans)
		  (begin
		    (vector-set! arcs-v n arcs)
		    (loop1 (+ n 1)))
		  (loop2 (cdr trans) (noeps-merge-1 (cdar trans) arcs))))
	    arcs-v)))))

; Preparer l'operateur pour digraph
(define sweep-op
  (let ((acc-min (lambda (rule1 rule2)
		   (cond ((not rule1)
			  rule2)
			 ((not rule2)
			  rule1)
			 (else
			  (min rule1 rule2))))))
    (lambda (acc1 acc2)
      (cons (acc-min (car acc1) (car acc2))
	    (acc-min (cdr acc1) (cdr acc2))))))

; Renumerotation des etats (#f pour etat a eliminer)
; Retourne (new-nbnodes . dict)
(define sweep-renum
  (lambda (dist-acc-v)
    (let* ((nbnodes (vector-length dist-acc-v))
	   (dict (make-vector nbnodes)))
      (let loop ((n 0) (new-n 0))
	(if (< n nbnodes)
	    (let* ((acc (vector-ref dist-acc-v n))
		   (dead? (equal? acc '(#f . #f))))
	      (if dead?
		  (begin
		    (vector-set! dict n #f)
		    (loop (+ n 1) new-n))
		  (begin
		    (vector-set! dict n new-n)
		    (loop (+ n 1) (+ new-n 1)))))
	    (cons new-n dict))))))

; Elimination des etats inutiles d'une liste d'etats
(define sweep-list
  (lambda (ss dict)
    (if (null? ss)
	'()
	(let* ((olds (car ss))
	       (news (vector-ref dict olds)))
	  (if news
	      (cons news (sweep-list (cdr ss) dict))
	      (sweep-list (cdr ss) dict))))))

; Elimination des etats inutiles d'une liste d'arcs
(define sweep-arcs
  (lambda (arcs dict)
    (if (null? arcs)
	'()
	(let* ((arc (car arcs))
	       (class (car arc))
	       (ss (cdr arc))
	       (new-ss (sweep-list ss dict)))
	  (if (null? new-ss)
	      (sweep-arcs (cdr arcs) dict)
	      (cons (cons class new-ss) (sweep-arcs (cdr arcs) dict)))))))

; Elimination des etats inutiles dans toutes les transitions
(define sweep-all-arcs
  (lambda (arcs-v dict)
    (let loop ((n (- (vector-length arcs-v) 1)))
      (if (>= n 0)
	  (begin
	    (vector-set! arcs-v n (sweep-arcs (vector-ref arcs-v n) dict))
	    (loop (- n 1)))
	  arcs-v))))

; Elimination des etats inutiles dans un vecteur
(define sweep-states
  (lambda (v new-nbnodes dict)
    (let ((nbnodes (vector-length v))
	  (new-v (make-vector new-nbnodes)))
      (let loop ((n 0))
	(if (< n nbnodes)
	    (let ((new-n (vector-ref dict n)))
	      (if new-n
		  (vector-set! new-v new-n (vector-ref v n)))
	      (loop (+ n 1)))
	    new-v)))))

; Elimination des etats inutiles
(define sweep
  (lambda (nl-start no-nl-start arcs-v acc-v)
    (let* ((digraph-arcs (sweep-mkarcs arcs-v))
	   (digraph-init acc-v)
	   (digraph-op sweep-op)
	   (dist-acc-v (digraph digraph-arcs digraph-init digraph-op))
	   (result (sweep-renum dist-acc-v))
	   (new-nbnodes (car result))
	   (dict (cdr result))
	   (new-nl-start (sweep-list nl-start dict))
	   (new-no-nl-start (sweep-list no-nl-start dict))
	   (new-arcs-v (sweep-states (sweep-all-arcs arcs-v dict)
				     new-nbnodes
				     dict))
	   (new-acc-v (sweep-states acc-v new-nbnodes dict)))
      (list new-nl-start new-no-nl-start new-arcs-v new-acc-v))))
