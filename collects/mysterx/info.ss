;; info.ss for mysterx collection

(lambda (request failure-thunk)
  (case request
    [(name) "MysterX"]
    [(compile-prefix) 
     (if (not (eq? (system-type) 'windows))
	 (begin
	   (fprintf (current-error-port) 
		    "Error: can't install MysterX on non-Windows machine~n")
	   (failure-thunk))
	 `(begin
	    (current-require-relative-collection (list "mysterx"))
	    (require-library "macro.ss")
	    (require-library "cores.ss")
	    (require-relative-library "mysterxu.ss")
	    (let* ([drives '(C D E F G H I J K L M N O P Q R S T U V W X Y Z)]
		   [winsys-dir 
		    (ormap 
		     (lambda (drive)
		       (let* ([win-sys-path 
			       (format "~a:\\WINDOWS\\SYSTEM" drive)]
			      [winnt-sys-path 
			       (format "~a:\\WINNT\\SYSTEM32" drive)])
			 (if (directory-exists? win-sys-path)
			     win-sys-path
			     (if (directory-exists? winnt-sys-path)
				 winnt-sys-path
				 #f)))) 
		     drives)])
	      (if winsys-dir
		  (for-each
		   (lambda (dll) 
		     (system
		      (format "~a\\REGSVR32 \"~a\\compiled\\native\\~a\"" 
			      winsys-dir (current-directory) dll)))
		   '(myspage.dll myssink.dll))
		  (fprintf (current-error-port) 
			   "Warning: Can't run REGSVR32 on libraries~n")))))]
    [else (failure-thunk)]))
