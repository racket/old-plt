(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/util.ss")
(require "private/refresh-util.ss")

(unit/sig ()
  (import servlet^)

  (define no-dir-page
    `(HTML
      (BODY
       (H1 ,(color-with "red"
			"CVS refresh error"))
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

  (define tmp-directory 
    (with-handlers
     ([void (lambda _ (send/finish no-dir-page))])
     (find/create-temporary-docs-dir)))

  (reset-progress-semaphore!) ; may have lost state via browser stop
  (reset-refresh-semaphore!) 

  (let ([hex-dir (hexify-string tmp-directory)])
    `(HTML 
      (FRAMESET ((ROWS "100,*"))
		(FRAMESET ((COLS "*,110"))
			  (FRAME ((NAME "refresh")
				  (SRC 
				   ,(format 
				     "/servlets/do-refresh.ss?tmp-dir=~a"
				     hex-dir))))
			  (FRAME ((NAME "stopper")
				  (SRC 
				   ,(format 
				     "/servlets/do-stop.ss?tmp-dir=~a"
				     hex-dir)))))
		(FRAME ((NAME "progress")
			(SRC "/servlets/progress.ss")))))))





