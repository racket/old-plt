;; info.ss for mysterx collection

(module info (lib "infotab.ss" "setup")
  (define name "MysterX")
  (define help-desk-message "Mz/Mr: (require (lib \"mysterx.ss\" \"mysterx\")")
  (define blurb
    (list
     "MysterX is an extension that lets you use Scheme to script "
     "ActiveX controls and other COM components under Windows. "
     "MysterX also has a programmable Web browser with support for "
     "Dynamic HTML.")))

#|


; unported info stuff


(lambda (request failure-thunk)
  (case request
    [(name) "MysterX"]
    [(compile-prefix) 
     (if (not (eq? (system-type) 'windows))
       (begin
	 (fprintf (current-error-port) 
		  "Error: can't install MysterX on non-Windows machine~n")
	 (failure-thunk))
       (let* ([dlls '("myspage.dll" "myssink.dll")]
	      [dll-paths 
		(map (lambda (dll)
		       (build-path (collection-path "mysterx")
				   "dlls" dll))
		     dlls)])
	 (if (not (andmap file-exists? dll-paths))
	     (begin
	       (fprintf (current-error-port) 
			"Warning: MysterX binaries not installed~n")
	       (failure-thunk))
	     `(begin
		(current-require-relative-collection (list "mysterx"))
		(require-library "macro.ss")
		(require-library "cores.ss")
		(require-relative-library "mysterxu.ss")
		(let ([winsys-dir (find-system-path 'sys-dir)])
		  (if winsys-dir	
		      (parameterize
		       ((current-directory
			 (build-path (collection-path "mysterx") "dlls")))
		       (for-each	
			(lambda (dll)
			  (system
			   (format "\"~a\" ~a" 
				   (build-path winsys-dir 
					       "REGSVR32")
				   dll)))
			',dlls))
		      (fprintf 
		       (current-error-port) 
		       "Warning: Can't run REGSVR32 on libraries~n")))))))]
	    [else (failure-thunk)]))
|#