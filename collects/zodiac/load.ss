(reference-library "macro.ss")
(reference-library "prettys.ss")

(reference "zsigs.ss")
(reference "sigs.ss")

; All this stuff needs to be disappeared.

(begin-elaboration-time
  (reference-library "file.ss")
  (define plt-home-directory
    (let ([plt (getenv "PLTHOME")])
      (normalize-path
	(or plt
	  (case (system-type)
	    [unix "/usr/local/lib/plt/"]
	    [windows "C:\\PLT"]
	    [else (let-values ([(base name dir?)
				 (split-path (current-directory))])
		    (if (string? base)
		      base
		      (current-directory)))]))))))
(require-library (begin-elaboration-time
		   (build-path plt-home-directory
		     "lib" "require.ss")))
(plt:require-library "sparams.ss")

(define zodiac:system@
  (reference-unit/sig "link.ss"))
