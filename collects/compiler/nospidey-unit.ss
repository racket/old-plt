
;; A compound unit for Scheme->C compilation without MrSpidey
;;  analysis.

(module nospidey-unit mzscheme
  (import (lib "unitsig.ss"))

  (import "sig.ss")
  (import "private/sig.ss")
  (import "private/base.ss")
  (import "private/spnoop.ss")

  (export compiler:nospidey@)

  (define compiler:nospidey@
    (compound-unit/sig
     (import (COMPILE : dynext:compile^)
	     (LINK : dynext:link^)
	     (DFILE : dynext:file^)
	     (OPTIONS : compiler:option^))
     (link
      [BASE : compiler:basic-link^ (base@
				    COMPILE
				    LINK
				    DFILE
				    OPTIONS
				    SPIDEY)]
      [SPIDEY : compiler:mrspidey^ (spidey-noop@)])
     (export (open ((BASE DRIVER) : compiler:inner^))))))
