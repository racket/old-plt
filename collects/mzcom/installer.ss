(module installer mzscheme
  (require (lib "process.ss"))
  (provide post-installer)
  (define post-installer
    (lambda (plt-home) 
      (if (not (eq? (system-type) 'windows))
	  (fprintf (current-error-port) 
		   "Warning: can't install MzCOM on non-Windows machine~n")
	  (let ([exe-path (build-path plt-home 
				      "collects" "mzcom" "mzcom.exe")]) 
	    (if (not (file-exists? exe-path))
		(fprintf (current-error-port) 
			 "Warning: MzCOM binary not installed~n")
		(parameterize 
		 ((current-directory (build-path plt-home "collects" "mzcom")))
		 (system (format "mzcom.exe /RegServer")))))))))
