(lambda (request)
  (case request
    [(name) "MrEd"]
    [(app-unit-library) "app.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-image-path) (build-path (require-library "iconpath.ss" "icons") "mred.gif")]
    [(splash-max) 93]
    [else (error 'mred-info "Unknown request: ~s" request)]))
