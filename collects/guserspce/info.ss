(let ([guserspace-info
       (lambda (what failure)
	 (case what
	   [(name) "Graphic Userspace"]
	   [(compile-prefix) 
	    '(begin
	       (require-library "refer.ss")
	       (require-library "coreflats.ss")
	       (require-library "errors.ss" "userspce")
	       (require-library "turtles.ss" "graphics")
	       (require-library "sig.ss" "mred")
	       (require-library "params.ss" "userspce")
	       (require-library "sig.ss" "guserspce"))]
	   [(compile-omit-files) (list "sig.ss"
				       "launcher-bootstrap-mred.ss")]
	   [(compile-elaboration-zos) (list "sig.ss")]
	   [else (failure)]))])
  guserspace-info)
