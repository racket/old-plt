(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
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
     (show "<HTML>")
     (show (xexpr->string
	    `(HEAD ,hd-css
		   ,(make-javascript
		       "function finish() {"
		       " document.forms[0].elements[1].disabled=\"true\""
		       "}"))))
     (show "<BODY onLoad=\"finish()\">")
     (show
      (xexpr->string
       `(FORM ((ACTION "/servlets/stop-refresh.ss")
	       (TARGET "_top")
	       (METHOD "POST"))
	      (INPUT ((TYPE "hidden")
		      (NAME "tmp-dir")
		      (VALUE ,tmp-directory)))
	      (INPUT ((TYPE "submit")
		      (NAME "stop")
		      (VALUE "Stop"))))))
     (show "<P></P>")
     (semaphore-wait refresh-semaphore)
     (show (xexpr->string home-page))
     (show "</BODY></HTML>"))))

