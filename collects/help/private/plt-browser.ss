(module plt-browser mzscheme
  (require (lib "file.ss"))

  (provide use-plt-browser?
           set-plt-browser!)

  (define switched-to-plt-browser #f)

  (define (set-plt-browser!)
    (set! switched-to-plt-browser #t))

  (define (use-plt-browser?)
    (or switched-to-plt-browser
	(eq? (get-preference 'external-browser (lambda () #f))
	     'plt))))
