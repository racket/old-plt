(lambda (request failure)
  (case request
    [(name) "Countdown"]
    [(app-unit-library) "link.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-max) 84]
    [(splash-depth) 6]
    [(compile-prefix) '(begin (require-library "sig.ss" "mred") (require-library "sig.ss" "countdwn"))]
    [(compile-omit-files) null]
    [(compile-elaboration-zos) (list "sig.ss")]

    [(install-collection)
     (lambda (x)
       (require-library "launcher.ss" "launcher")
       (make-mred-launcher (list "-magqvL" "run.ss" "countdwn")
			   (mred-program-launcher-path "Countdown")))]

    [(splash-image-path) (with-handlers ([void (lambda (x) "mred.gif")])
			   (build-path (collection-path "countdwn") "button.gif"))]
    [else (failure)]))
