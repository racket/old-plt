(lambda (request failure)
  (case request
    [(name) "Countdown"]
    [(app-unit-library) "link.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-max) 84]
    [(splash-depth) 6]
    [(compile-prefix) '(require-library "sig.ss" "countdwn")]
    [(compile-omit-files) (list "sig.ss")]
    [(compile-elaboration-zos) (list "sig.ss")]
    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")])
			   (build-path (collection-path "countdwn") "button.gif"))]
    [else (failure)]))
