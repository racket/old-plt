(module plt-browser mzscheme
  (require (lib "file.ss"))

  (provide use-plt-browser?
           set-plt-browser!)

  (define definitely-use-plt #f)

  (define (set-plt-browser!)
    (set! definitely-use-plt #t))

  (define (use-plt-browser?)
    (or definitely-use-plt
	(eq? (get-preference 'external-browser (lambda () #f))
	     'plt))))
