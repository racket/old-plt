
(lambda (request failure)
  (case request
    [(app-unit-library) "app.ss"]
    [(app-sig-library) "sig.ss"]
    [(splash-image-path) #f]
    [(name) "MrEd Demos"]
    [else (failure)]))
