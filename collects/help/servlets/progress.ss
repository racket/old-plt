(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "string-constant.ss" "string-constants")
	 (lib "xml.ss" "xml"))

(require "private/util.ss")
(require "private/refresh-util.ss")
(require "private/headelts.ss")

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

  (make-html-response/incremental
   (lambda (show)
     (show "<HTML>")
     (show (xexpr->string
	    `(HEAD ,hd-css 
                   ,@hd-links
		   (TITLE  "PLT manual download progress"))))
     (show "<BODY>")
     (show (xexpr->string 
	    `(H3 ,(color-with 
		   "blue" 
	           (string-constant plt:hd:refresh-installation-log)))))
     (show "<P></P>")
     (show "<PRE>")
     (show nl)
     (semaphore-wait progress-semaphore)

     (let ([port (get-progress-input-port)])
       (let loop ()
	 (let ([ln (read-line port)])
	   (unless (eof-object? ln)
		   (show ln)
		   (show nl)
		   (loop)))))
     (show "</PRE></BODY></HTML>")
     (show nl))))









