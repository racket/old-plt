(module check-text mzscheme
  (provide check-version)

  (require (lib "unitsig.ss"))

  (require "private/text-defs.ss")
  (require "private/go-check.ss")

  (define (check-version)
    (go-check 
	#f ; parent frame
	text-defs@)))
