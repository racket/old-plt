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
      ; Was invoke-open; needs define-values/invoke-unit for debugging:
      (invoke-unit/sig unit (program argv))
      (close-splash))))

