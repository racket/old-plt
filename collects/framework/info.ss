(letrec ([framework-info
	  (lambda (request fail)
	    (case request
	      [(name) "Framework"]
	      [(compile-prefix) `(begin
				   (require-library "sig.ss" "mred")
				   (require-library "frameworks.ss" "framework"))]
	      [(compile-omit-files)
	       (append
		(list "macro.ss" "mred-interfacess.ss" "tests.ss" "frameworks.ss");(framework-info 'compile-elaboration-zos (lambda () null))
		(list "gen-mred-interfaces.ss"
		      "gen-standard-menus.ss"
		      "standard-menus-items.ss"
		      "classhack.ss"))]
	      ;[(compile-elaboration-zos) null]
	      [else (fail)]))])
  framework-info)

