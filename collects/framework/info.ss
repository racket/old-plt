(lambda (request error)
  (case request
    [(name) "Framework"]
    [(compile-prefix) `(begin
			 (require-library "errortrace.ss" "errortrace")
			 (require-library "sig.ss" "mred")
			 (require-library "frameworks.ss" "framework"))]
    [(compile-elaboration-zos) (list "frameworks.ss")]
    [else (error)]))

