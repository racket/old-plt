(module plt-browser mzscheme
  (require (lib "file.ss"))

  (provide use-plt-browser?
           set-plt-browser!
	   help-browser-preference)

  (define switched-to-plt-browser #f)

  (define (set-plt-browser!)
    (set! switched-to-plt-browser #t))

  (define (help-browser-preference)
    (get-preference 'plt:help-browser
		    (lambda () 'external)))

  (define (use-plt-browser?)
    (or switched-to-plt-browser
	(eq? (help-browser-preference) 'plt))))


