(reference-library "file.ss")
(reference-library "macro.ss")
(reference-library "prettys.ss")
(when (begin-elaboration-time
	(and (defined? 'mcmicmac:interactive?)
	  mcmicmac:interactive?))
  (reference-library "prettyu.ss"))

(begin-elaboration-time
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
(require-library (build-path (begin-elaboration-time plt-home-directory)
		   "lib" "require.ss"))
(plt:require-library "sparams.ss")

(reference "zsigs.ss")
(reference "sigs.ss")

(when (begin-elaboration-time
	(and (defined? 'mcmicmac:interactive?)
	  mcmicmac:interactive?))
  (define zodiac:system@ (reference-unit/sig "link.ss"))
  (reference "invoke.ss"))
