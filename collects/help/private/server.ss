(module server mzscheme 

  (require (lib "internal-server.ss" "web-server")
           (lib "configuration.ss" "web-server")
           (lib "etc.ss")
           (lib "file.ss")
	   "browser-extensions.ss"
           "cookie.ss")
  
  (provide start-help-server)
  
  (define start-help-server
    (opt-lambda ([addl-browser-frame-mixin (lambda (x) x)])
      (let* ([configuration (build-developer-configuration (build-config-exp))]
             [hd-cookie (make-hd-cookie min-port #f #f #f #f #f #f)]
             [combined-browser-mixin
              (compose addl-browser-frame-mixin
                       (make-help-desk-frame-mixin hd-cookie))])
        (let-values ([(shutdown-server url-on-server-test extract-url-path url->string find-browser new-browser)
                      (internal-serve configuration min-port #f combined-browser-mixin)])
          (set-hd-cookie-shutdown-server! hd-cookie shutdown-server)
          (set-hd-cookie-url-on-server-test! hd-cookie url-on-server-test)
          (set-hd-cookie-extract-url-path! hd-cookie extract-url-path)
          (set-hd-cookie-url->string! hd-cookie url->string)
          (set-hd-cookie-find-browser! hd-cookie find-browser)
          (set-hd-cookie-new-browser! hd-cookie new-browser)
          hd-cookie))))
  
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