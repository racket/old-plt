(lambda (request failure)
  (case request
    [(name) "MrEd"]
    [(app-unit-library) "app.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")])
			   (build-path (collection-path "icons") "mred.gif"))]
    [(splash-max) 81]
    [(splash-depth) 6]
    [(compile-prefix) '(begin (require-library "sig.ss" "mred")
			      (require-library "wxs.ss" "system")
			      (require-library "invsig.ss" "system")
			      (require-library "debug.ss" "system")
			      (require-library "cmdlines.ss")
			      '(require-library "cores.ss"))]
    [(compile-omit-files) (list "wxs.ss" "invsig.ss" "debug.ss")]
    [else (failure)]))

