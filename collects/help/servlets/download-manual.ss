(require (lib "unitsig.ss")
	 (lib "xml.ss" "xml")
         (lib "servlet-helpers.ss" "web-server")
         (lib "servlet-sig.ss" "web-server")
	 (lib "url.ss" "net"))

(require "private/util.ss"
         "private/refresh-util.ss"
         "private/remote.ss")

(unit/sig ()
  (import servlet^)

  (check-remote 	
   send/finish
   (url-path (request-uri initial-request)))

  (make-html-response/incremental
   (lambda (show)
     (reset-progress-semaphore!) 
     (let* ([tmp-directory 	
	     (with-handlers	
	      ([void (lambda _ 
		       (send/finish (no-download-dir-page)))])
	      (find/create-temporary-docs-dir))]
	    [bindings (request-bindings initial-request)]
	    [get-binding (lambda (sym) 
			   (extract-binding/single sym bindings))]
	    [manual (get-binding 'manual)]
	    [label (get-binding 'label)]
	    [manual-exists? (file-exists?
			     (build-path
			      (collection-path "doc")
			      manual
			      "index.htm"))])
       (show "<HTML>"
	     (xexpr->string
	      `(HEAD ,hd-css
		     (TITLE "PLT manual installation progress")
		     ,refresh-stop-javascript))
	     refresh-stop-body-tag)
       (show 
	(xexpr->string
	 (refresh-stop-form tmp-directory
			    "Installing " label
			    'nbsp 'nbsp)))
       (let-values ([(iport oport) (make-pipe)])
		   ; this critical section is unprotected
		   ; getting semaphores to work right
		   ;  here is nigh impossible
		   (set-progress-input-port! iport)		   
		   (set-progress-output-port! oport)		   
		   (semaphore-post progress-semaphore)
		   (doc-collections-changed)
		   (show "Downloading ..." "<BR>")
		   (download-known-doc tmp-directory manual)
		   (show "Deleting existing files ..." "<BR>")
		   (delete-known-doc tmp-directory manual)
		   (show "Installing files ..." "<BR>")
		   (run-setup-plt tmp-directory manual)
		   (delete-directory/r tmp-directory)
		   (close-output-port oport)
		   (close-input-port iport))
       (show "<P>"
	     (xexpr->string
	      `(A ((HREF ,(string-append "/doc/" manual "/")))
		  "Click here"))
	     " to view manual"
	     "<P>"
	     (xexpr->string
	      home-page))
       (show "</BODY></HTML>")
       (when manual-exists?
	     (delete-known-doc tmp-directory manual))))))
