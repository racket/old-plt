(lambda (request failure)
  (case request
    [(name) "Countdown"]
    [(app-unit-library) "link.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-max) 84]
    [(splash-depth) 6]
    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")])
			   (build-path (collection-path "countdwn") "button.gif"))]
    [else (failure)]))
