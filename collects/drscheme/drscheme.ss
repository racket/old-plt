(when (getenv "MREDDEBUG")
  (parameterize ([current-eventspace (make-eventspace)])
    (let* ([f (make-object frame% "Quit")]
           [h (make-object horizontal-panel% f)])
      (send (make-object button% "Quit" h (lambda (x y) (exit))) stretchable-width #t)
      (make-object grow-box-spacer-pane% h)
      (send f reflow-container)
      (let-values ([(w h) (get-display-size)])
        (send f move 
              (- w (send f get-width))
              (- h (send f get-height))))
      (send f show #t)))
  (require-library "errortrace.ss" "errortrace") (error-print-width 200)
  (current-load (let ([ol (current-load)]) (lambda (x) (printf "~a~n" x) (ol x)))))

;(define argv (vector "Cupertino:Desktop Folder:tmp.ss"))

(define-values (get-dropped-files shutdown-splash close-splash)
  ((require-library "splash.ss" "framework")
   (build-path (collection-path "icons") "plt.gif")
   "DrScheme"
   81))

(require-relative-library "drsig.ss")

(let ([unit (require-relative-library "link.ss")])
  (shutdown-splash)
  (global-define-values/invoke-unit/sig drscheme^
                                        unit
                                        #f
                                        (program argv get-dropped-files))
  (close-splash))
(yield (make-semaphore 0))
