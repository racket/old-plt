(let ([userspace-info
       (lambda (what failure)
	 (case what
	   [(name) "Userspace"]
	   [(compile-prefix) 
	    '(begin
	       (read-case-sensitive #t)
	       (require-library "refer.ss")
	       (require-library "sig.ss" "userspce"))]
	   [(compile-omit-files) (list "userspcs.ss" "ricedefs.ss" "sig.ss")]
	   [(compile-elaboration-zos) (list "sig.ss")]
	   [else (failure)]))])
  userspace-info)
