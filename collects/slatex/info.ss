(lambda (request fail)
  (case request
    ((name) "SLaTeX")
    ((install-collection)
     (lambda (plt-home) 
       (unless (file-exists? (build-path (collection-path "slatex") "compiled" "slatex.zo"))
	 (parameterize ([current-namespace (make-namespace)]
			[current-output-port (make-output-port void void)]
			[current-directory (collection-path "slatex" "slatex-code")])
	   (require-library "slaconfg.scm" "slatex" "slatex-code")))
       (require-library "launcher.ss" "launcher")
       (make-mzscheme-launcher 
	(list "-qge" 
	      "(require-library \"slatex-launcher.scm\"
				\"slatex\")")
	(mzscheme-program-launcher-path "SLaTeX"))))
    (else (fail))))
