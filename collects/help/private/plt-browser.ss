(module plt-browser mzscheme
  (require (lib "file.ss"))

  (provide use-plt-browser?
           set-plt-browser!
	   external-browser-preference
	   help-browser-preference
	   get-browser-param
           set-browser-param!)

  (define switched-to-plt-browser #f)

  (define (set-plt-browser!)
    (set! switched-to-plt-browser #t))

  (define (help-browser-preference)
    (get-preference 'plt:help-browser
		    (lambda () 'external)))

  (define (external-browser-preference)
    (get-preference 'external-browser
		    (lambda () #f)))

  (define (use-plt-browser?)
    (or switched-to-plt-browser
	(eq? (help-browser-preference) 'plt)))

  (define browser-param #f)

  (define (get-browser-param)
    browser-param)
  
  (define (set-browser-param! v)
    (set! browser-param v)))



