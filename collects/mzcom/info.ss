;; info.ss for mzcom collection

(module info (lib "infotab.ss" "setup")
  (define name "MzCOM")
  (define blurb
    (list
      "MzCOM is a COM class that makes Scheme available to any COM client."))
  (define release-version "200alpha1")
  (define release-iteration "0"))

#|
(lambda (request failure-thunk)
  (case request
    [(name) "MzCOM"]
    [(compile-prefix) 
     (if (not (eq? (system-type) 'windows))
	 (begin
	   (fprintf (current-error-port) 
		    "Error: can't install MzCOM on non-Windows machine~n")
	   (failure-thunk))
	 (let ([exe-path (build-path (collection-path "mzcom") "mzcom.exe")]) 
	   (if (not (file-exists? exe-path))
	       (begin
		 (fprintf (current-error-port) 
			  "Warning: MzCOM binary not installed~n")
		 (failure-thunk))
	       `(parameterize 
		 ((current-directory (build-path (collection-path "mzcom"))))
		 (system (format "mzcom.exe /RegServer"))))))]
    [(blurb)
     (list
      "MzCOM is a COM class that makes Scheme available to any COM client.")]
    [else (failure-thunk)]))
|#