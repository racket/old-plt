(module server-config mzscheme

  (require (lib "file.ss")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (provide build-config-exp
           make-hd-cookie 
	   hd-cookie->port
	   hd-cookie->exit-proc
	   hd-cookie->browser
           hd-cookie->browser-mixin
           hd-cookie->internal-url-test
           set-hd-cookie-port!
           set-hd-cookie-exit-proc!
           set-hd-cookie-browser!
           set-hd-cookie-internal-url-test!
	   hd-cookie?
           min-port
           max-port)

  (define-struct hd-cookie (port exit-proc browser internal-url-test browser-mixin) (make-inspector))
  (define hd-cookie->port hd-cookie-port)
  (define hd-cookie->exit-proc hd-cookie-exit-proc)
  (define hd-cookie->browser hd-cookie-browser)
  (define hd-cookie->browser-mixin hd-cookie-browser-mixin)
  (define hd-cookie->internal-url-test hd-cookie-internal-url-test)
  
  (define min-port 8000)
  (define max-port 8900)

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
    `((port ,min-port)
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
          ;(file-not-found-message ,(build-path servlet-root "servlets" "home.ss"))
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
      (virtual-host-table)))))







