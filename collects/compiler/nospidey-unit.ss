
;; A compound unit for Scheme->C compilation without MrSpidey
;;  analysis.

(module nospidey-unit mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "file-sig.ss" "dynext")
	  (lib "link-sig.ss" "dynext")
	  (lib "compile-sig.ss" "dynext"))

  (require "sig.ss")
  (require "private/sig.ss")

  (require "private/base.ss")
  (require "private/spnoop.ss")

  (provide compiler-linked@)

  (define compiler-linked@
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
