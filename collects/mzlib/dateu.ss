
(reference-library "refer.ss")

(reference-library "dates.ss")
(reference-library "functios.ss")

(define mzlib:date@ (reference-library-unit/sig "dater.ss"))

#|

    '(let* ((s (current-seconds))
	    (t (sleep 1))
	    (s2 (- s (* 353 3600 24)))
	    (s3 (- s (* 332 3600 24)))
	    (offset2 (date- (seconds->date s2) (seconds->date s)))
	    (offset3 (date- (seconds->date s3) (seconds->date s))))
       (write (date->string (seconds->date s)))
       (newline)
       (write (date->string (seconds->date s2)))
       (newline)
       (write (date->string (seconds->date s3)))
       (newline)
       (write (date-offset->string offset2))
       (newline)
       (write (date-offset->string offset3))
       (newline)
       (write (map (lambda (m) (list (days-per-month 1996 m) (days-per-month 1997 m)))
		   (list 1 2 3 4 5 6 7 8 9 10 11 12)))
       (newline))

|#
