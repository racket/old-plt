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

 `(HTML
   (HEAD ,hd-css
	 (TITLE "Help Desk frame toggle")
	 (META ((HTTP-EQUIV "refresh")
		(CONTENT ,(format "0;URL=/servlets/home.ss")))))
   (BODY 
    "If this page does not refresh, "
    (A ((HREF "/servlets/home.ss")) "click here"))))
