#!/bin/sh

string=? ; exec mzscheme -qr $0

(require-library "sig.ss" "games" "paint-by-numbers")

(require-library "errortrace.ss" "errortrace")

(define counter
  (unit/sig ()
    (import paint-by-numbers:problem^
	    paint-by-numbers:all-problems^)

    (define total 0)
    (define total-missing 0)

    (define (check-set problems set-name)
      (let ([missing (apply + (map (lambda (problem) (if (problem-solution problem) 0 1)) problems))])
	(set! total (+ (length problems) total))
	(set! total-missing (+ missing total-missing))
	(unless (zero? missing)
	  (printf "~a missing ~a~n" set-name missing))))


    (for-each check-set problemss set-names)
    (printf "missing ~a of ~a~n" total-missing total)))

(invoke-unit/sig
 (compound-unit/sig (import)
   (link
    [problem : paint-by-numbers:problem^ ((require-library "problem.ss" "games" "paint-by-numbers"))]
    [all : paint-by-numbers:all-problems^ ((require-library "all-problems.ss" "games" "paint-by-numbers") problem)]
    [counter : () (counter problem all)])
   (export)))
