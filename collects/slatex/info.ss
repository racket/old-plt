(lambda (request fail)
  (case request
    ((name) "SLaTeX")
    [(blurb)
     (list "SLaTeX is an pre-processor for LaTeX that formats Scheme code. "
	   "For more information, see "
	   `(tt () "slatxdoc.dvi")
	   " in the "
	   `(tt () ,(build-path (collection-path "slatex") "slatex-code"))
	   " directory on this machine.")]
    ((install-collection)
     (lambda (plt-home) 
       (unless (file-exists? (build-path (collection-path "slatex") "compiled" "slatexsrc.zo"))
         (let ([slatex-code-directory (build-path (collection-path "slatex") "slatex-code")]
               [compiled-directory (build-path (collection-path "slatex") "compiled")])
           (parameterize ([current-namespace (make-namespace)]
                          [current-output-port (make-output-port void void)]
                          [current-directory slatex-code-directory])
             (require-library "slaconfg.scm" "slatex" "slatex-code"))
           (unless (directory-exists? compiled-directory)
             (make-directory compiled-directory))
           (copy-file (build-path slatex-code-directory "slatex.scm") ; this file is actually a .zo file
                      (build-path compiled-directory "slatexsrc.zo"))))
       (require-library "launcher.ss" "launcher")
       (make-mzscheme-launcher 
	(list "-qge" 
	      "(require-library \"slatex-launcher.scm\"
				\"slatex\")")
	(mzscheme-program-launcher-path "SLaTeX"))))
    (else (fail))))
