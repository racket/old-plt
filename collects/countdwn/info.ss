(lambda (request failure)
  (case request
    [(name) "Countdown"]
    [(app-unit-library) "link.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-max) 84]
    [(splash-depth) 6]
    [(compile-prefix) '(require-library "sig.ss" "countdwn")]
    [(compile-omit-files) (list "sig.ss"
				"remind.ss"
				"link.ss"
				"during.ss"
				"before.ss"
				"after.ss")]
    [(compile-elaboration-zos) (list "sig.ss")]

    [(install-collection)
     (lambda (x)
       (require-library "launcher.ss" "launcher")
       (make-mred-launcher (list "-magqvL" "run.ss" "countdwn")
			   (mred-program-launcher-path "Countdown")))]

    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")])
			   (build-path (collection-path "countdwn") "button.gif"))]
    [else (failure)]))
