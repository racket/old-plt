(require (lib "unitsig.ss")
         (lib "servlet-helpers.ss" "web-server")
         (lib "servlet-sig.ss" "web-server")
         (lib "start.ss" "help" "private"))

(unit/sig ()
  (import servlet^)
  ; signal that external browser succeeded
  (post-start-semaphore)
  (let* ([bindings (request-bindings initial-request)]
         [url (with-handlers
	       ([void (lambda _ "/servlets/home.ss")])
	       (extract-binding/single 'url bindings))])
    (redirect-to url)))


