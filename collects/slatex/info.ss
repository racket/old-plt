
(module info (lib "infotab.ss" "setup")
  (define name "SLaTeX")
  (define help-desk-message
    "Mz/Mr: (require (lib \"slatex.ss\" \"slatex\"))")
  (define blurb
    (list "SLaTeX is an pre-processor for LaTeX that formats Scheme code. "
	  "For more information, see "
	  `(tt () "slatxdoc.dvi")
	  " in the "
	  `(tt () ,(build-path (collection-path "slatex") "slatex-code"))
	  " directory on this machine."))

  (define compile-omit-files '("slatexsrc.ss"))
  
  (define install-collection "installer.ss"))
