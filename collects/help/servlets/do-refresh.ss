(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "search.ss" "help" "private")
	 (lib "docpos.ss" "help" "private")
	 (lib "xml.ss" "xml"))

(require "private/util.ss")
(require "private/hd-css.ss")
(require "private/refresh-util.ss")

(unit/sig ()
  (import servlet^)

  (define tmp-directory (extract-binding/single 'tmp-dir
			 (request-bindings initial-request)))

  (make-html-response/incremental
   (lambda (show)
     (let* ([make-action
	     (lambda (action name)
	       (lambda (doc)
		 (let ([doc-name (car doc)]
		       [doc-label (cdr doc)])
		   (show name " " doc-label "<BR>")
		   (action tmp-directory doc-name))))]
	    [downloader (make-action download-known-doc "Downloading")]
	    [deleter (make-action delete-known-doc "Deleting")]
	    [installer (make-action run-setup-plt "Installing")]
	    [looper (lambda (f)
		      (for-each f known-docs))])
       (doc-collections-changed)
       (show "<HTML>")
       (show (xexpr->string `(HEAD ,hd-css)))
       (show "<BODY><PRE>")
       (let-values ([(iport oport) (make-pipe)])
         (set-progress-input-port! iport)		   
	 (set-progress-output-port! oport)		   
	 (semaphore-post progress-semaphore)
	 (for-each looper 
		   (list downloader deleter installer))
	 (close-output-port oport))
       (delete-directory/r tmp-directory)
       (show (xexpr->string `(B "Done refreshing CVS manuals")))
       (show "</PRE></BODY></HTML>")
       (semaphore-post refresh-semaphore)))))
