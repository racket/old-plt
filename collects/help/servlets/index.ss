(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "servlet-helpers.ss" "web-server"))

(require "private/util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define search-height
    (get-pref/default 'search-height search-height-default))

  `(HTML 
    (HEAD ,hd-css
	  (TITLE "PLT Help Desk"))
    (FRAMESET ((ROWS ,(string-append search-height ",*")))
	      (FRAME ((NAME "search")
		      (SRC "/servlets/search.ss")
		      (MARGINHEIGHT "2")
		      (MARGINWIDTH "2")))
	      (FRAME ((NAME "main")
		      (SRC "/servlets/main.ss"))))))











