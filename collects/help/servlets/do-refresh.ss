(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "help-desk-mz.ss" "help")
	 (lib "string-constant.ss" "string-constants")
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
	     (lambda (action format-string)
	       (lambda (doc)
		 (let ([doc-name (car doc)]
		       [doc-label (cdr doc)])
		   (show (format format-string doc-label) "<BR>")
		   (action tmp-directory doc-name))))]
	    [downloader (make-action download-known-doc 
				     (string-constant refresh-downloading))]
	    [deleter (make-action delete-known-doc 
				  (string-constant refresh-deleting))]
	    [installer (make-action run-setup-plt 
				    (string-constant refresh-installing))]
	    [looper (lambda (f)
		      (for-each f known-docs))])
       (doc-collections-changed)
       (show "<HTML>")
       (show (xexpr->string 
	      `(HEAD ,hd-css (TITLE "PLT manual download progress"))))
       (show "<BODY><PRE>")
       (let-values ([(iport oport) (make-pipe)])
         (set-progress-input-port! iport)		   
	 (set-progress-output-port! oport)		   
	 (semaphore-post progress-semaphore)
	 (for-each looper 
		   (list downloader deleter installer))
	 (close-output-port oport))
       (delete-directory/r tmp-directory)
       (show (xexpr->string `(B ,(string-constant 
				  refresh-done))))
       (show "</PRE></BODY></HTML>")
       (semaphore-post refresh-semaphore)))))


