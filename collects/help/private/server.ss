(module server mzscheme 

  (require (lib "web-server.ss" "web-server")
	   (lib "util.ss" "web-server")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (provide start-help-server
	   hd-cookie->port
	   hd-cookie?)

  (define-struct hd-cookie (port exit-proc) (make-inspector))
  (define hd-cookie->port hd-cookie-port)

  (define min-port 8000)
  (define max-port 8900)

  (define (get-free-port)
    (let loop ([curr-port min-port])
      (if (> curr-port max-port)
	  (error 'help-desk "no port available for Help Desk server")
	  (if (with-handlers ; #t iff no server on this port
	       ([void (lambda _ #t)])
	       (let-values 
		([(i o) (tcp-connect "127.0.0.1" curr-port)])
		(close-output-port o)
		(close-input-port i)
		#f))
	      curr-port
	      (loop (add1 curr-port))))))

  (define help-desk-port (get-free-port))

  (define (start-help-server)
    (let* ([configuration
	   (load-developer-configuration
	    (extract-flag 
	     'config '() 
	     (build-path (collection-path "help")
			 "server-configuration")))])
      ; restrict connections to localhost
      (make-hd-cookie help-desk-port  	
		      (serve configuration help-desk-port))))) ; "127.0.0.1")))))











