(require-library "errortrace.ss" "errortrace") (error-print-width 100)

;(current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x))))

(define argv (vector "Cupertino:Desktop Folder:tmp.ss"))

(define-values (shutdown-splash close-splash)
  ((require-library "splash.ss" "framework")
   (build-path (collection-path "icons") "plt.gif")
   "DrScheme"
   81
   5))

;(require-library "refer.ss")

(require-relative-library "drsig.ss")

(let ([unit (require-relative-library "link.ss")])
  (shutdown-splash)
  (global-define-values/invoke-unit/sig framework^
                                        unit
                                        #f
                                        (program argv))
  (close-splash))

(yield (make-semaphore 0))