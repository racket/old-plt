(require (lib "unitsig.ss")
	 (lib "string.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "string-constant.ss" "string-constants")
         (lib "xml.ss" "xml"))

(require "private/search-pane.ss")
(require "private/util.ss")

(unit/sig ()
  (import servlet^)

  (adjust-timeout! +inf.0)

  (define search-bg
    (get-pref/default 'search-bg search-bg-default))

  `(HTML 
    (HEAD (TITLE "PLT Help Desk search"))
    (BODY ((BGCOLOR ,search-bg))
	,(search-pane))))

