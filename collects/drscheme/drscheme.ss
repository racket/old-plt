(define-values (change-splash-message shutdown-splash close-splash)
  ((require-library "splash.ss" "framework")
   (build-path (collection-path "icons") "plt.gif")
   "DrScheme"
   100
   5))

(require-relative-library "drsig.ss")
(let ([unit (require-relative-library "link.ss")])
  (shutdown-splash)
  (invoke-open-unit/sig unit
			#f
			(program argv)))
