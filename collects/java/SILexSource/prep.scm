; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

;
; Divers pre-traitements avant l'ecriture des tables
;

; Passe d'un arc multi-range a une liste d'arcs mono-range
(define prep-arc->sharcs
  (lambda (arc)
    (let* ((range-l (car arc))
	   (dest (cdr arc))
	   (op (lambda (range) (cons range dest))))
      (map op range-l))))

; Compare des arcs courts selon leur premier caractere
(define prep-sharc-<=
  (lambda (sharc1 sharc2)
    (class-<= (caar sharc1) (caar sharc2))))

; Remplit les trous parmi les sharcs avec des arcs "erreur"
(define prep-fill-error
  (lambda (sharcs)
    (let loop ((sharcs sharcs) (start 'inf-))
      (cond ((class-= start 'inf+)
	     '())
	    ((null? sharcs)
	     (cons (cons (cons start 'inf+) 'err) (loop sharcs 'inf+)))
	    (else
	     (let* ((sharc (car sharcs))
		    (h (caar sharc))
		    (t (cdar sharc)))
	       (if (class-< start h)
		   (cons (cons (cons start (- h 1)) 'err) (loop sharcs h))
		   (cons sharc (loop (cdr sharcs)
				     (if (class-= t 'inf+)
					 'inf+
					 (+ t 1)))))))))))

; ; Passe d'une liste d'arcs a un arbre de decision
; ; 1ere methode: seulement des comparaisons <
; (define prep-arcs->tree
;   (lambda (arcs)
;     (let* ((sharcs-l (map prep-arc->sharcs arcs))
; 	   (sharcs (apply append sharcs-l))
; 	   (sorted-with-holes (merge-sort sharcs prep-sharc-<=))
; 	   (sorted (prep-fill-error sorted-with-holes))
; 	   (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
; 	   (table (list->vector (map op sorted))))
;       (let loop ((left 0) (right (- (vector-length table) 1)))
; 	(if (= left right)
; 	    (cdr (vector-ref table left))
; 	    (let ((mid (quotient (+ left right 1) 2)))
; 	      (list (car (vector-ref table mid))
; 		    (loop left (- mid 1))
; 		    (loop mid right))))))))

; Passe d'une liste d'arcs a un arbre de decision
; 2eme methode: permettre des comparaisons = quand ca adonne
(define prep-arcs->tree
  (lambda (arcs)
    (let* ((sharcs-l (map prep-arc->sharcs arcs))
	   (sharcs (apply append sharcs-l))
	   (sorted-with-holes (merge-sort sharcs prep-sharc-<=))
	   (sorted (prep-fill-error sorted-with-holes))
	   (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
	   (table (list->vector (map op sorted))))
      (let loop ((left 0) (right (- (vector-length table) 1)))
	(if (= left right)
	    (cdr (vector-ref table left))
	    (let ((mid (quotient (+ left right 1) 2)))
	      (if (and (= (+ left 2) right)
		       (= (+ (car (vector-ref table mid)) 1)
			  (car (vector-ref table right)))
		       (eqv? (cdr (vector-ref table left))
			     (cdr (vector-ref table right))))
		  (list '=
			(car (vector-ref table mid))
			(cdr (vector-ref table mid))
			(cdr (vector-ref table left)))
		  (list (car (vector-ref table mid))
			(loop left (- mid 1))
			(loop mid right)))))))))

; Determine si une action a besoin de calculer yytext
(define prep-detect-yytext
  (lambda (s)
    (let loop1 ((i (- (string-length s) 6)))
      (cond ((< i 0)
	     #f)
	    ((char-ci=? (string-ref s i) #\y)
	     (let loop2 ((j 5))
	       (cond ((= j 0)
		      #t)
		     ((char-ci=? (string-ref s (+ i j))
				 (string-ref "yytext" j))
		      (loop2 (- j 1)))
		     (else
		      (loop1 (- i 1))))))
	    (else
	     (loop1 (- i 1)))))))

; Note dans une regle si son action a besoin de yytext
(define prep-set-rule-yytext?
  (lambda (rule)
    (let ((action (get-rule-action rule)))
      (set-rule-yytext? rule (prep-detect-yytext action)))))

; Note dans toutes les regles si leurs actions ont besoin de yytext
(define prep-set-rules-yytext?
  (lambda (rules)
    (let loop ((n (- (vector-length rules) 1)))
      (if (>= n 0)
	  (begin
	    (prep-set-rule-yytext? (vector-ref rules n))
	    (loop (- n 1)))))))
