(lambda (request error)
  (case request
    [(name) "Framework"]
    [(install-collection)
     (lambda (_)
       (require-library "launcher.ss" "launcher")
       (make-mred-launcher (list "-mvaqgL" "test.ss" "framework")
			   (mred-program-launcher-path "Test Framework")))]
    [else (error)]))

