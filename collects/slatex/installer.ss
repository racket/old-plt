
(module installer mzscheme
  (require (lib "launcher.ss" "launcher"))

  (define installer
    (lambda (plt-home) 
      (unless (file-exists? (build-path (collection-path "slatex") "compiled" "slatexsrc.zo"))
	(let ([slatex-code-directory (build-path (collection-path "slatex") "slatex-code")]
	      [compiled-directory (build-path (collection-path "slatex") "compiled")])
	  (parameterize ([current-namespace (make-namespace)]
			 [current-output-port (make-output-port void void)]
			 [current-directory slatex-code-directory])
	    (load (build-path slatex-code-directory "slaconfg.scm")))
	  (unless (directory-exists? compiled-directory)
	    (make-directory compiled-directory))
	  (copy-file (build-path slatex-code-directory "slatex.scm") ; this file is actually a .zo file
		     (build-path compiled-directory "slatexsrc.zo"))))
      (let ([meta-make-mzscheme-launcher
             (lambda (launcher name)
               (make-mzscheme-launcher 
                (list "-qge" 
                      (string-append "(require (lib \"" launcher "\" \"slatex\"))"))
                (mzscheme-program-launcher-path name)))])
        (meta-make-mzscheme-launcher "slatex-launcher.scm" "SLaTeX")
        (meta-make-mzscheme-launcher "pdf-slatex-launcher.scm" "PDF SLaTeX"))))
  (provide installer))
