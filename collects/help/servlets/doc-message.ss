(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/headelts.ss")

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

  (let ([bindings (request-bindings initial-request)])
    `(HTML 
      (HEAD ,hd-css
	    ,@hd-links
	    (TITLE "PLT collection message"))
      (BODY 
       ,(format-collection-message 
	 (extract-binding/single 'msg bindings))
       (HR)))))

