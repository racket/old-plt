(module server mzscheme 

  (require (lib "etc.ss")
	   (lib "web-server.ss" "web-server")
	   (lib "util.ss" "web-server")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (require (lib "external.ss" "help" "servlets" "private"))

  (provide start-help-server
	   hd-cookie->port
	   hd-cookie?)

  (define-struct hd-cookie (port exit-proc) (make-inspector))
  (define hd-cookie->port hd-cookie-port)

  (define min-port 8000)
  (define max-port 8900)

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

  (define start-help-server
    (opt-lambda ([use-port #f][external-connections? #f]) 
      (let* ([configuration
	      (load-developer-configuration
	       (extract-flag 
		'config '() 
		(build-path (collection-path "help")
			    "server-configuration")))]
	     [help-desk-port (get-free-port use-port)])
	(set-box! external-box external-connections?)
	(make-hd-cookie 
	 help-desk-port  	
	 (if external-connections?
	     (serve configuration help-desk-port)
	     (serve configuration help-desk-port "127.0.0.1")))))))













