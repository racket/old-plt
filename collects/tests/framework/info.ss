(lambda (request response)
  (case request
    [(name) "Framework"]
    [(install-collection)
     (lambda (_)
       (require-library "launcher.ss" "launcher")
       (make-mred-launcher (list "-aqge-" "(require-library \"receive-sexps.ss\" \"tests\" \"framework\")")
			   (mred-program-launcher-path
			    (require-library "receive-sexps-name.ss"
					     "tests"
					     "framework"))))]))
