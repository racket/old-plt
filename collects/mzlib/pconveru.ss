;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gann Bierner                                -=)
;; gbierner@owlnet.rice.edu                     (=-             \ O
;;                                                _O             \_-)---
;; File: pconveru.ss                           (=-_/             /\
;;                                                 /\
;;
;; This file contains code which formats an expression to show all sharing
;; within it. Call `print-convert' to convert a value.
;; It takes 2 optional arguments.  The first is a boolean value, just-circular.
;; If true, sharing will only be shown for circularity.  The default is #f
;; where all sharing is shown.  The first argument to share:print-all is, of
;; course, the expression to convert.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reference-library "refer.ss")

(reference-library "pconvers.ss")
(reference-library "strings.ss")
(reference-library "functios.ss")

(define mzlib:print-convert@ (reference-library-unit/sig "pconverr.ss"))

#|

(let ()
  (define-compound-sigfunctor (linked@ mzlib:print-convert^)
    (import)
    (with (mzlib:print-convert@ mzlib:string@ mzlib:function@
				mzlib:print-convert-hooks@)
	  (mzlib:string@)
	  (mzlib:function@)
	  (mzlib:print-convert-hooks@))
    (export mzlib:print-convert@))

  (invoke-open-functor (sigfunctor->functor linked@) (mzlib:print-convert@)))

|#

