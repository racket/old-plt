(module installer mzscheme
  (provide installer)

  (require "mkindex.ss")

  (define installer
    (lambda (path) 
	(create-index-file))))



