(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "xml.ss" "xml")
         (lib "help-desk-mz.ss" "help"))

(require "private/hd-css.ss")
(require "private/util.ss")
(require "private/refresh-util.ss")

(unit/sig ()
  (import servlet^)

  (let* ([bindings (request-bindings initial-request)]
	 [get-binding (lambda (sym) (extract-binding/single sym bindings))]
	 [tmp-directory (get-binding 'tmpdir)]
	 [label (get-binding 'label)]
	 [manual (get-binding 'manual)])
    (make-html-response/incremental
     (lambda (show)
       (show "<HTML>"
	     (xexpr->string
	      `(HEAD ,hd-css
	             (TITLE "PLT manual installation progress")
		     ,(make-javascript
		       "function hideStop() {"
		       " document.forms[0].elements[1].disabled=\"true\""
		       "}")))
	      "<BODY onLoad=\"hideStop()\">")
       (show (xexpr->string
	      `(FORM ((ACTION "/servlets/stop-refresh.ss")
		      (TARGET "_top")
		      (METHOD "POST"))
		     "Installing "
		     ,label
		     'nbsp 'nbsp
		     (INPUT ((TYPE "hidden")
			     (NAME "tmp-dir")
			     (VALUE ,(hexify-string tmp-directory))))
		     (INPUT ((TYPE "submit")
			     (ID "stop")
			     (NAME "stop")
			     (VALUE "Stop"))))))
       (let-values ([(iport oport) (make-pipe)])
         (set-progress-input-port! iport)		   
         (set-progress-output-port! oport)		   
	 (semaphore-post progress-semaphore)
         (doc-collections-changed)
	 (download-known-doc tmp-directory manual)
         (delete-known-doc tmp-directory manual)
         (run-setup-plt tmp-directory manual)
	 (delete-directory/r tmp-directory)
	 (close-output-port oport)
	 (close-input-port iport))
       (show (xexpr->string
	      `(A ((HREF ,(string-append "/doc/" manual "/"))
		   (TARGET "main"))
		  "Click here"))
	     " to view manual")
       (show "</BODY></HTML>")))))

