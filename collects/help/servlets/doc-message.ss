(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (let ([bindings (request-bindings initial-request)])
    `(HTML 
      (HEAD ,hd-css)
      (BODY 
       (B ((STYLE "color:green")) 
	  ,(extract-binding/single 'msg bindings))
       (HR)))))

