(define (start-drscheme)
  (let-values ([(change-splash-message shutdown-splash close-splash)
		((require-library "splash.ss" "framework")
		 (build-path (collection-path "icons") "plt.gif")
		 "DrScheme"
		 123
		 5)])
    (require-relative-library "drsig.ss")
    (let ([unit (require-relative-library "link.ss")])
      (change-splash-message "Invoking...")
      (shutdown-splash)
      (invoke-open-unit/sig unit #f (program argv))
      (close-splash))))

