(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "xml.ss" "xml"))

(require "private/util.ss")
(require "private/refresh-util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (make-html-response/incremental
   (lambda (show)
     (show "<HTML>")
     (show (xexpr->string
	    `(HEAD ,hd-css (TITLE "PLT manual download progress"))))
     (show "<BODY>")
     (show (xexpr->string 
	    `(H3 ,(color-with 
		   "blue" 
		   "Installation log"))))
     (show "<P></P>")
     (show "<PRE>")
     (show nl)
     (semaphore-wait progress-semaphore)

     (let ([logfile "c:/tmp/log.start"])
       (when (file-exists? logfile)
	     (delete-file logfile))
       (with-output-to-file logfile
	 (lambda () (printf "starting at ~a~n" (current-seconds)))))

     (let ([port (get-progress-input-port)])
       (let loop ()
	 (let ([ln (read-line port)])
	   (unless (eof-object? ln)
		   (show ln)
		   (show nl)
		   (loop)))))
     (show "</PRE></BODY></HTML>")
     (show nl))))









