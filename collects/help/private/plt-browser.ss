(module plt-browser mzscheme
  (require (lib "file.ss"))

  (provide use-plt-browser?)

  (define (help-browser-preference)
    (get-preference 'plt:help-browser
		    (lambda () 'external)))

  (define (use-plt-browser?)
    (eq? (help-browser-preference) 'plt)))



