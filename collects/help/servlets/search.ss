(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server"))

(require "private/search-pane.ss")
(require "private/util.ss")
(require "private/headelts.ss")

(unit/sig ()
  (import servlet^)

  (adjust-timeout! +inf.0)

  (report-errors-to-browser send/finish)

  (define (search-bg)
    (get-pref/default 'plt:hd:search-bg search-bg-default))

  (define search-string
    (with-handlers ([void (lambda _ "")])
      (extract-binding/single 'search-string 
			      (request-bindings
			       initial-request))))

  `(HTML 
    (HEAD (TITLE "PLT Help Desk search")
	  ,hd-css
          ,@hd-links)
    (BODY ((STYLE ,(string-append "background-color:" (search-bg))))
	  ,(search-pane search-string))))
