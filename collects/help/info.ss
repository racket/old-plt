(lambda (request failure)
  (case request
    [(name) "Help"]
    [(compile-prefix) `(begin
			 (require-library "sig.ss" "mred")
			 (require-library "sig.ss" "help"))]
    [(compile-omit-files) (list "sig.ss" "manuals.ss")]
    [(compile-elaboration-zos) (list "sig.ss")]
    [else (failure)]))

