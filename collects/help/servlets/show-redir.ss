(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/util.ss")
(require "private/refresh-util.ss")

(unit/sig ()
  (import servlet^)

  (let* ([bindings (request-bindings initial-request)]
	 [url (extract-binding/single 'url bindings)])
    `(HTML
      (BODY
       "If your browser does not refresh, please "
       (A ((HREF ,url) (TARGET "main")) "click here")))))



