; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

;
; Fonction pour reconstituer le fichier silex.scm a partir des
; differents modules
;

(define update
  (lambda ()
    (let ((out-port (open-output-file "silex.scm")))
      (display "; Copyright (C) 1997 Danny Dube', " out-port)
      (display "Universite' de Montre'al." out-port)
      (newline out-port)
      (display "; All rights reserved." out-port)
      (newline out-port)
      (display "; SILex 1.0." out-port)
      (newline out-port)
      (newline out-port)
      (for-each
       (lambda (in-file)
	 (display "; Module " out-port)
	 (display in-file out-port)
	 (display "." out-port)
	 (newline out-port)
	 (let ((in-port (open-input-file in-file)))
	   (let loop ((c (read-char in-port)))
	     (if (eof-object? c)
		 (begin
		   (newline out-port)
		   (close-input-port in-port))
		 (begin
		   (write-char c out-port)
		   (loop (read-char in-port)))))))
       '("util.scm"
	 "action.l.scm"
	 "class.l.scm"
	 "macro.l.scm"
	 "regexp.l.scm"
	 "string.l.scm"
	 "multilex.scm"
	 "lexparser.scm"
	 "re2nfa.scm"
	 "noeps.scm"
	 "sweep.scm"
	 "nfa2dfa.scm"
	 "prep.scm"
	 "output.scm"
	 "output2.scm"
	 "main.scm"))
      (close-output-port out-port))))
