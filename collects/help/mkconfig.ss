(module mkconfig mzscheme

  (provide create-server-config-file)

  (require (lib "file.ss"))

  (define config-file (build-path (collection-path "help")
				  "server-configuration"))

  (define (create-server-config-file)
    (when (file-exists? config-file)
	  (delete-file config-file))
    (with-output-to-file config-file
      (lambda ()
	(write (build-config-exp)))))

  (define (build-config-exp)
    (let* ([help-path (collection-path "help")]
	   [doc-path (collection-path "doc")]
	   [file-root (normalize-path 
		       (build-path doc-path 'up))]
	   [host-root (build-path help-path "web-root")]
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
      (virtual-host-table)))))


