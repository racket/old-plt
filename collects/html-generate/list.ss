#| -*-Scheme-*-

$Id: list.ss,v 1.1.1.1 1999/04/06 21:39:40 clements Exp $

Copyright (c) 1988-93 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;(define (for-each procedure first . rest)
;  (mapping-procedure for-each begin unspecific procedure first rest))

;(define (map procedure first . rest)
;  (mapping-procedure map cons '() procedure first rest))

(define map*         #f)
(define append-map   #f)
(define append-map*  #f)
(define append-map!  #f)
(define append-map*! #f)

(define-macro mapping-procedure
	   (lambda (name combiner initial-value procedure first rest)
	     (let ((name (symbol->string name)))
	       `(IF (NULL? ,rest)
		    (LET 1-LOOP ((LIST ,first))
		      (IF (PAIR? LIST)
			  (,combiner (,procedure (CAR LIST))
				     (1-LOOP (CDR LIST)))
			  (BEGIN
			    (IF (NOT (NULL? LIST))
				(ERROR:WRONG-TYPE-ARGUMENT ,first "list" (QUOTE ,name)))
			    ,initial-value))
		    (LET ((LISTS (CONS ,first ,rest)))
		      (LET N-LOOP ((LISTS* LISTS))
			(LET PARSE-CARS
			    ((LISTS LISTS)
			     (LISTS* LISTS*)
			     (CARS '())
			     (CDRS '()))
			  (COND ((NULL? LISTS*)
				 (,combiner (APPLY ,procedure (REVERSE! CARS))
					    (N-LOOP (REVERSE! CDRS))))
				((PAIR? (CAR LISTS*))
				 (PARSE-CARS (CDR LISTS)
					     (CDR LISTS*)
					     (CONS (CAAR LISTS*) CARS)
					     (CONS (CDAR LISTS*) CDRS)))
				(ELSE
				 (IF (NOT (NULL? (CAR LISTS*)))
				     (ERROR:WRONG-TYPE-ARGUMENT (CAR LISTS) "list"
								(QUOTE ,name)))
				 ,initial-value))))))))))

;(define (for-each procedure first . rest)
;  (mapping-procedure for-each begin unspecific procedure first rest))

;(define (map procedure first . rest)
;  (mapping-procedure map cons '() procedure first rest))

(set! map*
      (lambda (initial-value procedure first . rest)
	(mapping-procedure map* cons initial-value procedure first rest)))

(set! append-map 
      (lambda (procedure first . rest)
	(mapping-procedure append-map append '() procedure first rest)))

(set! append-map* 
      (lambda (initial-value procedure first . rest)
	(mapping-procedure append-map* append initial-value procedure first rest)))

(set! append-map! 
      (lambda (procedure first . rest)
	(mapping-procedure append-map! append! '() procedure first rest)))

(set! append-map*! 
      (lambda (initial-value procedure first . rest)
	(mapping-procedure append-map*! append! initial-value procedure first rest)))

(define (cons* . stuff)
  (cond ((null? stuff) '())
	((null? (cdr stuff)) (car stuff))
	(else (let loop ((remainder stuff))
		(if (null? (cdr remainder))
		    remainder
		    (cons (car remainder) (loop (cdr remainder))))))))