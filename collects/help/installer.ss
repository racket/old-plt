(module installer mzscheme
  (provide installer)

  (require "mkindex.ss")
  (require "mkconfig.ss")

  (define installer
    (lambda (path) 
	(create-index-file)
	(create-server-config-file))))



