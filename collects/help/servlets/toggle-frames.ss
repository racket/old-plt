(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "string-constant.ss" "string-constants")
         (lib "xml.ss" "xml"))

(require "private/util.ss")
(require "private/hd-css.ss")
(require "private/external.ss")

(unit/sig ()
  (import servlet^)

  (put-prefs (list 'use-frames)
	     (list (if (use-frames?) "false" "true")))	

  (redirect-to "/servlets/home.ss"))
