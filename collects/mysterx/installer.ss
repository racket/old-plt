(module installer mzscheme
  (require (lib "process.ss"))

  (define post-installer
    (lambda (mx-path) 
      (if (not (eq? (system-type) 'windows))
	  (fprintf (current-error-port) 
		   "Warning: can't install MysterX on non-Windows machine~n")
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
			    (if (system
				 (string-append 
				  (build-path winsys-dir "REGSVR32.EXE")
				  " /s " ; silent mode
				  dll))
				(printf "MysterX: Registered library ~a~n"
					dll)
				(fprintf (current-error-port)
					 "MysterX: Unable to register library ~a~n"
					 dll)))
			  dlls))
			(fprintf
			 (current-error-port) 
			 "Warning: Can't run REGSVR32 on libraries~n")))))))))
  (provide installer))
