#!/bin/sh

string=? ; exec mzscheme -qr $0

;; this builds raw-kajitani.ss from raw-kajitani
;; raw-kajitani.ss is used in build-problems.ss

(require-library "pretty.ss")
(require-library "function.ss")
(require-library "errortrace.ss" "errortrace")

(define raw-kajitani (call-with-input-file (build-path (collection-path "games" "paint-by-numbers") "raw-kajitani")
		       read))

(define counters (make-hash-table))

(define (get-name kaj)
  (let ([prefix (format "~ax~a" (car (car kaj)) (cadr (car kaj)))]
	[tag (string->symbol prefix)]
	[num (hash-table-get
	      counters
	      tag
	      (lambda ()
		(hash-table-put! counters tag 0)
		0))])
    (hash-table-put! counter tag (+ num 1))
    (format "~a ~a" prefix (+ num 1))))

(define kajitani-sets
  (let ([ht (make-hash-table)])
    (for-each
     (lambda (kaj-set)
       (let ([tag (string->symbol (format "~ax~a" (car (car kaj-set)) (cadr (car kaj-set))))]
	     [rows/cols (list (caddr (car kaj-set)) (cdr kaj-set))])
	 (hash-table-put!
	  ht
	  tag
	  (cons
	   rows/cols
	   (hash-table-get
	    ht
	    tag
	    (lambda ()
	      null))))))
     raw-kajitani)
    (hash-table-map ht (lambda (x l) (list x (reverse l))))))
     
(define (build-solutionless-kajitani kaj-set)
  (list
   (format "Kajitani ~a" (car kaj-set))
   (format "k~a" (car kaj-set))
   (map
    (let ([n 0])
      (lambda (kaj)
	(set! n (+ n 1))
	(list (format "~a (~a)" (car kaj) n)
	      (cadr (cadr kaj))
	      (car (cadr kaj)))))
    (cadr kaj-set))))

(call-with-output-file (build-path (collection-path "games" "paint-by-numbers")
				   "raw-kajitani.ss")
  (lambda (port)
    (pretty-print
     (quicksort
      (map build-solutionless-kajitani kajitani-sets)
      (lambda (s1 s2)
	(string<=? (car s1) (car s2))))
     port))
  'truncate)
