(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "help-desk-mz.ss" "help")
	 (lib "string-constant.ss" "string-constants")
	 (lib "xml.ss" "xml")
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
     (reset-progress-semaphore!) ; may have lost state via browser stop
     (let* ([tmp-directory 
	     (with-handlers
	      ([void (lambda _ 
		       (send/finish no-dir-page))])
	      (find/create-temporary-docs-dir))]
	    [make-action
	     (lambda (action format-string)
	       (lambda (doc)
		 (let ([doc-name (car doc)]
		       [doc-label (cdr doc)])
		   (show (format format-string doc-label) "<BR>")
		   (action tmp-directory doc-name))))]
	    [downloader (make-action 
			 download-known-doc 
			 (string-constant plt:hd:refresh-downloading))]
	    [deleter (make-action 
		      delete-known-doc 
		      (string-constant plt:hd:refresh-deleting))]
	    [installer (make-action 
			run-setup-plt 
			(string-constant plt:hd:refresh-installing))]
	    [looper (lambda (f)
		      (for-each f known-docs))])
       (doc-collections-changed)
       (show "<HTML>")
       (show (xexpr->string 
	      `(HEAD ,hd-css (TITLE "Refresh PLT manuals")
		     ,refresh-stop-javascript)))
       (show refresh-stop-body-tag)
       (show (xexpr->string (refresh-stop-form tmp-directory)))
       (show "<PRE>")
       (let-values ([(iport oport) (make-pipe)])
		   (set-progress-input-port! iport)		   
		   (set-progress-output-port! oport)		   
		   (semaphore-post progress-semaphore)
		   (for-each looper 
			     (list downloader deleter installer))
		   (close-output-port oport))
       (delete-directory/r tmp-directory)
       (show (xexpr->string `(B ,(string-constant plt:hd:refresh-done))))
       (show "</PRE>")
       (show "<P>")
       (show (xexpr->string home-page))
       (show "</BODY></HTML>")))))
















































































