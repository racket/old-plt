; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

;
; Fonction pour reconstituer le module output2.scm a partir du fichier
; multilex.scm
;

(define update
  (lambda ()
    (let ((in-port (open-input-file "multilex.scm"))
	  (out-port (open-output-file "output2.scm")))
      (display ";" out-port)
      (newline out-port)
      (display "; Fonction de copiage du fichier run-time" out-port)
      (newline out-port)
      (display ";" out-port)
      (newline out-port)
      (newline out-port)
      (display "(define out-print-run-time-lib" out-port)
      (newline out-port)
      (display "  (lambda (port)" out-port)
      (newline out-port)
      (display "    (display \"; *** This file start\" port)" out-port)
      (newline out-port)
      (display "    (display \"s with a copy of the \" port)" out-port)
      (newline out-port)
      (display "    (display \"file multilex.scm ***\" port)" out-port)
      (newline out-port)
      (display "    (newline port)" out-port)
      (newline out-port)
      (display "    (display \"" out-port)
      (let loop ((c (read-char in-port)))
	(if (eof-object? c)
	    (begin
	      (display "\" port)))" out-port)
	      (newline out-port)
	      (close-input-port in-port)
	      (close-output-port out-port))
	    (begin
	      (cond ((char=? c #\")
		     (write-char #\\ out-port)
		     (write-char #\" out-port))
		    ((char=? c #\\)
		     (write-char #\\ out-port)
		     (write-char #\\ out-port))
		    (else
		     (write-char c out-port)))
	      (loop (read-char in-port))))))))
