
(module info (lib "infotab.ss" "setup")
  (define name "SLaTeX")

  ;(define tools (list (list "slatex-lang.ss")))
  ;(define tool-names (list "SLaTeX Language"))
  
  (define help-desk-message
    "Mz/Mr: (require (lib \"slatex.ss\" \"slatex\"))")
  (define blurb
    (list "SLaTeX is an pre-processor for LaTeX that formats Scheme code. "
	  "For more information, see "
	  `(tt () "slatxdoc.dvi")
	  " in the "
	  `(tt () ,(build-path (collection-path "slatex") "slatex-code"))
	  " directory on this machine."))
  
  (define install-collection "installer.ss")
  (define compile-omit-files '("slatex.scm")))
