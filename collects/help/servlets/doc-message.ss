(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (let ([bindings (request-bindings initial-request)])
    `(HTML 
      (HEAD ,hd-css
	    (TITLE "PLT collection message"))
      (BODY 
       ,(format-collection-message 
	 (extract-binding/single 'msg bindings))
       (HR)))))

