(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "servlet-helpers.ss" "web-server"))	

(require "private/read-doc.ss")

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

  (let* ([bindings (request-bindings initial-request)]
	 [offset (with-handlers
		  ((void (lambda _ #f)))
		  (string->number	 
		   (extract-binding/single 'offset bindings)))])
    (read-doc (extract-binding/single 'file bindings)
      (extract-binding/single 'caption bindings)
      (extract-binding/single 'name bindings)
      offset)))


