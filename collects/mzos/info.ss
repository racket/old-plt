(lambda (request failure)
  (case request
    [(name) "MzOS"]
    [(compile-prefix) '(begin (require-library "wxs.ss" "system")
			      (require-library "invsig.ss" "system")
			      (require-library "minsig.ss" "mred"))]
    [(compile-omit-files) null]
    [else (failure)]))
