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
	    (let ([winsys-dir (find-system-path 'sys-dir)])
	      (if winsys-dir	
		  (for-each	
		   (lambda (dll)	 
		     (system
		      (format "~a\\REGSVR32 \"~a\\dlls\\~a\"" 
			      winsys-dir (current-directory) dll)))
		   '(myspage.dll myssink.dll))
		  (fprintf (current-error-port) 
			   "Warning: Can't run REGSVR32 on libraries~n")))))]
    [(blurb)
     (list
      "MysterX is an extension that lets you use Scheme to script "
      "ActiveX controls and other COM components under Windows. "
      "MysterX also has a programmable Web browser with support for 
       Dynamic HTML. "	
      "See the "
      `(a ((href ,(format "file:~a" (build-path (collection-path "mysterx") 
	"doc.txt"))))
          "doc.txt")
      " file for more information.")]
    [else (failure-thunk)]))
