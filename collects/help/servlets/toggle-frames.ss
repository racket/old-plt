(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server"))

(require "private/util.ss")
(require "private/hd-css.ss")
(require "private/external.ss")

(unit/sig ()
  (import servlet^)

  (put-prefs (list 'plt:hd:use-frames)
	     (list (if (use-frames?) "false" "true")))	

  (redirect-to "/servlets/home.ss"))
