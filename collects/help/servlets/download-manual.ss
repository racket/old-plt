(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/util.ss")
(require "private/hd-css.ss")
(require "private/refresh-util.ss")

(unit/sig ()
  (import servlet^)

  (define no-dir-page
    `(HTML
      (HEAD ,hd-css
	    (TITLE "PLT manual download error"))
      (BODY
       (H1 ,(color-with "red"
			"Manual download error"))
       (P)
       (B ,(color-with "red"
		       "Could not create temporary directory"
		       `(P)
		       "Please clean out the "
		       `(TT ,refresh-docs-dir-base "*")
		       " subdirectories of "
		       (find-system-path 'temp-dir)))
	(P)
	,home-page)))

  (let* ([tmp-directory 
	  (with-handlers
	   ([void (lambda _ (send/finish no-dir-page))])
	   (find/create-temporary-docs-dir))]
	 [bindings (request-bindings initial-request)]
	 [get-binding (lambda (sym) (extract-binding/single sym bindings))]
	 [manual (get-binding 'manual)]
	 [label (get-binding 'label)])

    (reset-progress-semaphore!) 

    `(HTML 
      (HEAD ,hd-css)
      (FRAMESET ((ROWS "100,*"))
		(FRAME ((NAME "install")
			(SRC ,(format 
				(string-append
				 "/servlets/show-install.ss?"
				 "tmpdir=~a&"
				 "label=~a&"
				 "manual=~a")
			       (hexify-string tmp-directory)
			       (hexify-string label)
			       (hexify-string manual)))))
		(FRAME ((NAME "progress")
			(SRC "/servlets/progress.ss")))))))


  




     


