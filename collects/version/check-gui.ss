(module check-gui mzscheme
  (provide check-version)

  (require "private/gui-defs.ss")
  (require "private/go-check.ss")

  (define (check-version)
    (go-check 
     #f ; parent frame
     gui-defs@)))

