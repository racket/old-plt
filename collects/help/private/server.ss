(module server mzscheme 
  (require (lib "web-server-unit.ss" "web-server")
           (lib "sig.ss" "web-server")
           (lib "configuration.ss" "web-server")
           (lib "etc.ss")
           (lib "file.ss")
           (lib "unitsig.ss")
           (lib "tcp-sig.ss" "net")
           "cookie.ss")
  
  (provide get-serve-ports)
  
  (define (get-serve-ports)
    (let ([config (build-developer-configuration (build-config-exp))])
      (invoke-unit/sig
       (compound-unit/sig
         (import (t : net:tcp^))
         (link
          [c : web-config^ (config)]
          [s : web-server^ (web-server@ t c)]
          [m : () ((unit/sig ()
                     (import web-server^)
                     serve-ports)
                   s)])
         (export))
       net:tcp^)))
  
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