(lambda (request)
  (case request
    [(name) "Countdown"]
    [(app-unit-library) "link.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")])
			   (build-path (collection-path "countdwn") "button.gif"))]
    [(splash-max) 100]
    [else (error 'countdown-info "Unknown request: ~s" request)]))
