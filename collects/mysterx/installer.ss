(module installer mzscheme
  (require (lib "process.ss"))

  (define installer
    (lambda (plt-home) 
      (if (not (eq? (system-type) 'windows))
	  (fprintf (current-error-port) 
		   "Error: can't install MysterX on non-Windows machine~n")
	  (let* ([dlls '("myspage.dll" "myssink.dll")]
		 [dll-path-list (list (collection-path "mysterx")
				      "private" "compiled" "native" 
				      "win32" "i386")]
		 [dll-paths 
		  (map (lambda (dll)
			 (apply build-path (append dll-path-list (list dll))))
		       dlls)])
	    (if (not (andmap file-exists? dll-paths))
		(fprintf (current-error-port) 
			 "Warning: MysterX binaries not installed~n")
		(begin
		  (let ([winsys-dir (find-system-path 'sys-dir)])
		    (if winsys-dir	
			(parameterize
			 ((current-directory
			   (apply build-path dll-path-list)))
			 (for-each	
			  (lambda (dll)
			    (system
			     (format "~a ~a" 
				     (build-path winsys-dir 
						 "REGSVR32")
				     dll)))
			  dlls))
			(fprintf 
			 (current-error-port) 
			 "Warning: Can't run REGSVR32 on libraries~n")))))))))
  (provide installer))
