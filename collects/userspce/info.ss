(let ([userspace-info
       (lambda (what failure)
	 (case what
	   [(name) "Userspace"]
	   [(compile-prefix) 
	    '(begin
	       (require-library "refer.ss")
	       (require-library "coreflats.ss")
	       (require-library "errors.ss" "userspce")
	       (require-library "params.ss" "userspce")
	       (require-library "sig.ss" "userspce"))]
	   [(compile-omit-files) (list "sig.ss" "errors.ss" "params.ss" "ricedefs.ss"
				       "launcher-bootstrap.ss"
				       "advancedr.ss"
				       "launcher-bootstrap-mred.ss"
				       "launcher-bootstrap-mzscheme.ss")]
	   [(compile-elaboration-zos) (list "sig.ss")]
	   [else (failure)]))])
  userspace-info)
