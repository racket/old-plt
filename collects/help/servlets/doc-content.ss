(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/read-lines.ss")

(unit/sig ()
  (import servlet^)

  (let* ([bindings (request-bindings initial-request)]
	 [file (extract-binding/single 'file bindings)]
	 [caption (extract-binding/single 'caption bindings)]
	 [offset (with-handlers 
		  ((void (lambda _ #f)))
		  (string->number 
		   (extract-binding/single 'offset bindings)))])
    (read-lines file caption offset)))


















