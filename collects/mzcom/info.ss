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
	 '(let ([fn ".\\mzcom.exe"])
	    (when (file-exists? fn)
		  (system (string-append fn " /RegServer")))))]
    [(blurb)
     (list
      "MzCOM is a COM class that makes Scheme available to any COM client.")]
    [else (failure-thunk)]))
