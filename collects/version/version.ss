(module version mzscheme
  (provide check-version)

  (require "private/gui-defs.ss")
  (require "private/go-check.ss")

  (define (check-version parent-frame)
    (go-check 
	parent-frame 
	gui-defs@)))














