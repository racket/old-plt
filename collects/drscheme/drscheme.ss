(define debug? #t)

(define start-drscheme-expression
  '(let-values ([(change-splash-message shutdown-splash close-splash)
		 ((require-library "splash.ss" "framework")
		  (build-path (collection-path "icons") "plt.gif")
		  "DrScheme"
		  123
		  5)])
     (require-relative-library "drsig.ss")
     (let ([unit (require-relative-library "link.ss")])
       (shutdown-splash)
       (invoke-open-unit/sig unit
			     #f
			     (program argv)))))

(if debug?
    (begin
      (thread
       (lambda ()
	 (let* ([f (let loop ([n 10])
		     (cond
		      [(get-top-level-focus-window) => (lambda (x) x)]
		      [(zero? n) (error 'drscheme.ss "didn't find frame after 5 seconds")]
		      [else
		       (sleep/yield 1/2)
		       (loop (- n 1))]))]
		[canvas
		 (let loop ([f f])
		   (cond
		    [(is-a? f editor-canvas%) f]
		    [(is-a? f area-container<%>) (ormap loop (send f get-children))]
		    [else (error 'drscheme.ss "couldn't find editor")]))]
		[text (send canvas get-editor)]
		[port (open-output-string)]
		[event (make-object key-event%)])
	   (write start-drscheme-expression port)
	   (send text insert (get-output-string port))
	   (send event set-key-code #\return)
	   (send text on-char event))))

      (graphical-read-eval-print-loop))
    (eval start-drscheme-expression))

