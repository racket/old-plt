(letrec ([mred-info
	  (lambda (request failure)
	    (case request
	      [(name) "MrEd Collection"]
	      [(compile-prefix) '(begin
				   (read-case-sensitive #t)
				   (require-library "refer.ss")
				   (require-library "wxs.ss" "system")
				   (require-library "sig.ss" "mred")
				   (require-library "debug.ss" "system"))]
	      [(compile-omit-files)
	       (append
		(list "wxr.ss" "cppmode.ss" "wxs.ss"
		      ;"linkwx.ss" ;; okay to compile this now
		      "exapp.ss")
		(mred-info 'compile-elaboration-zos failure))]
	      [(compile-elaboration-zos-prefix)
	       '(begin
		  (read-case-sensitive #t)
		  (require-library "refer.ss"))]
	      [(compile-elaboration-zos)
	       (list "stsigs.ss" "minsig.ss" "sig.ss")]
	      [else (failure)]))])
  mred-info)
