(module external-server mzscheme
  (require (lib "etc.ss")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (require (lib "web-server.ss" "web-server"))

  (require "server-config.ss")

  (require (lib "external.ss" "help" "servlets" "private"))

  (provide external-start-help-server
           wait-for-connection)

  (define (get-free-port use-port)
    (let ([hi-port (or use-port max-port)])
      (let loop ([curr-port (or use-port min-port)])
	(if (> curr-port hi-port)
	    (error "Help Desk: port not available for server")
	    (if (with-handlers ; #t iff no server on this port
		 ([void (lambda _ #t)])
		 (let-values 
		  ([(i o) (tcp-connect "127.0.0.1" curr-port)])
		  (close-output-port o)
		  (close-input-port i)
		  #f))
		curr-port
		(loop (add1 curr-port)))))))

  (define (wait-for-connection port)
    (let loop ()
      (with-handlers
       ([void (lambda _ (sleep 1) (loop))])
       (let-values 
	([(iport oport) (tcp-connect "127.0.0.1" port)])
	(sleep 1)
	(close-output-port oport)
	(close-input-port iport)))))

  (define external-start-help-server
    (lambda (addl-browser-frame-mixin use-port external-connections?) 
      (let ([configuration
             (build-developer-configuration
              (build-config-exp))]
	    [help-desk-port (get-free-port use-port)])
        (set-box! external-box external-connections?)
        (make-hd-cookie 
         help-desk-port  	
         (if external-connections?
             (serve configuration help-desk-port)
             (serve configuration help-desk-port "127.0.0.1"))
         #f
         addl-browser-frame-mixin)))))











