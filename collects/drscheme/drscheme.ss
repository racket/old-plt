(require-library "errortrace.ss" "errortrace")
(error-print-width 100)

;(current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x))))

(require-library "refer.ss")

(begin-elaboration-time
 (require-library "invoke.ss")
 (require-library "macro.ss")
 (require-relative-library "drsig.ss"))

(let-values ([(shutdown-splash close-splash)
	      ((require-library "splash.ss" "framework")
	       (build-path (collection-path "icons") "plt.gif")
	       "DrScheme"
	       81
	       5)])
  (require-relative-library "drsig.ss")
  (let ([unit (require-relative-library "link.ss")])
    (shutdown-splash)
    (global-define-values/invoke-unit/sig framework^
					  unit
					  #f
					  (program argv))
    (close-splash)))

(yield (make-semaphore 0))