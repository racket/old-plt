(require (lib "unitsig.ss")
	 (lib "string.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "xml.ss" "xml"))

(require "private/search-pane.ss")
(require "private/util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (adjust-timeout! +inf.0)

  (define (search-bg)
    (get-pref/default 'plt:hd:search-bg search-bg-default))

  `(HTML 
    (HEAD (TITLE "PLT Help Desk search")
	  ,hd-css)
    (BODY ((STYLE ,(string-append "background-color:" (search-bg))))
	,(search-pane))))
