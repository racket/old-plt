(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "url.ss" "net"))

(require "private/util.ss"
 	 "private/remote.ss")

(unit/sig ()
  (import servlet^)

  (check-remote 	
   send/finish
   (url-path (request-uri initial-request)))

  (report-errors-to-browser send/finish)

  (put-prefs (list 'plt:hd:use-frames)
	     (list (if (use-frames?) "false" "true")))	

  (redirect-to "/servlets/home.ss"))
