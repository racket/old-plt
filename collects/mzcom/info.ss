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
	 `(system ".\\mzcom.exe /RegServer"))]
    [(blurb)
     (list
      "MzCOM is a COM class that makes Scheme available to any COM client. "
      "See the "
      `(a ((href ,(format "file:~a" (build-path (collection-path "mzcom") "doc.txt"))))
          "doc.txt")
      " file for more information.")]
    [else (failure-thunk)]))
