
(begin-elaboration-time
 (require-library "invoke.ss")
 (require-relative-library "drsig.ss"))

(define (start-drscheme)
  (let-values ([(shutdown-splash close-splash)
		((require-library "splash.ss" "framework")
		 (build-path (collection-path "icons") "plt.gif")
		 "DrScheme"
		 123
		 5)])
    (require-relative-library "drsig.ss")
    (let ([unit (require-relative-library "link.ss")])
      (shutdown-splash)
      (global-define-values/invoke-unit/sig ((unit fw : framework^))
		unit
		#f
		(program argv))
      (close-splash))))

