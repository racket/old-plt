(letrec ([userspce-info
	  (lambda (request failure)
	    (case request
	      [(name) "Userspace"]
	      [(clean) '("compiled")]
	      [(compile-prefix) 
	       '(begin
		  (require-library "sig.ss" "userspace"))]
	      [(compile-omit-files)
	       (append
		(userspce-info 'compile-elaboration-zos failure)
		(list "init-namespacer.ss"))]
	      [(compile-elaboration-zos-prefix)
	       '(begin
		  (require-library "refer.ss"))]
	      [(compile-elaboration-zos)
	       (list "sig.ss")]
	      [else (failure)]))])
  userspce-info)
