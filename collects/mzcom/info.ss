;; info.ss for mzcom collection

(lambda (request failure-thunk)
  (case request
    [(name) "MzCOM"]
    [(compile-prefix) 
     (if (not (eq? (system-type) 'windows))
	 (begin
	   (fprintf (current-error-port) 
		    "Error: can't install MzCOM on non-Windows machine~n")
	   (failure-thunk))
	 `(begin
	    (let ([winsys-dir (find-system-path 'sys-dir)])
	      (if winsys-dir	
		  (for-each	
		   (lambda (dll)	 
		     (system
		      (format "~a\\REGSVR32 \"~a\\~a\"" 
			      winsys-dir (current-directory) dll)))
		   '(mzcom.dll))
		  (fprintf (current-error-port) 
			   "Warning: Can't run REGSVR32 on libraries~n")))))]
    [else (failure-thunk)]))
