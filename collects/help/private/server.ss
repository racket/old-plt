(module server mzscheme 

  (require (lib "etc.ss")
	   (lib "file.ss")
	   (lib "web-server.ss" "web-server")
	   (lib "util.ss" "web-server")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (require (lib "external.ss" "help" "servlets" "private"))

  (provide start-help-server
	   hd-cookie->port
	   hd-cookie?
           wait-for-connection)

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

  (define (build-config-exp)
    (let* ([build-normal-path
	    (lambda args
	      (normalize-path
	       (apply build-path args)))]
	   [help-path (build-normal-path (collection-path "help"))]
	   [doc-path (build-normal-path help-path 'up "doc")]
	   [file-root (build-normal-path doc-path 'up)]
	   [host-root (build-normal-path help-path "web-root")]
	   [servlet-root help-path])
    `((port 8000)
      (max-waiting 40)
      (initial-connection-timeout 30)
      (default-host-table
	(host-table
	 (default-indices "index.html" "index.htm")
	 (log-format parenthesized-default)
	 (messages
	  (servlet-message "servlet-error.html")
	  (authentication-message "forbidden.html")
	  (servlets-refreshed "servlet-refresh.html")
	  (passwords-refreshed "passwords-refresh.html")
	  (file-not-found-message "not-found.html")
	  (protocol-message "protocol-error.html"))
	 (timeouts
	  (default-servlet-timeout 120)
	  (password-connection-timeout 300)
	  (servlet-connection-timeout 86400)
	  (file-per-byte-connection-timeout 1/20)
	  (file-base-connection-timeout 30))
	 (paths
	  (configuration-root "conf")
	  (host-root ,host-root)
	  (log-file-path "log")
	  (file-root ,file-root)
	  (servlet-root ,servlet-root)
	  (password-authentication "passwords"))))
      (virtual-host-table))))

  (define (wait-for-connection port)
    (let loop ()
      (with-handlers
       ([void (lambda _ (sleep 1) (loop))])
       (let-values 
	([(iport oport) (tcp-connect "127.0.0.1" port)])
	(sleep 1)
	(close-output-port oport)
	(close-input-port iport)))))

  (define start-help-server
    (opt-lambda ([use-port #f][external-connections? #f]) 
      (let* ([configuration
	      (build-developer-configuration
	       (build-config-exp))]
	     [help-desk-port (get-free-port use-port)])
	(set-box! external-box external-connections?)
	(make-hd-cookie 
	 help-desk-port  	
	 (if external-connections?
	     (serve configuration help-desk-port)
	     (serve configuration help-desk-port "127.0.0.1")))))))













