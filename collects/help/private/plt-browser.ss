(module plt-browser mzscheme
  (require (lib "file.ss"))

  (provide use-plt-browser?)

  (define (use-plt-browser?)
    (eq? (get-preference 'external-browser (lambda () #f))
	 'plt)))
