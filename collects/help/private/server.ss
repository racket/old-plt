(module server mzscheme 

  (require (lib "etc.ss")
	   (lib "file.ss")
	   (lib "browser.ss" "net"))

  (require "plt-browser.ss"
	   "server-config.ss"
	   "external-server.ss"
	   "internal-server.ss")

  (provide start-help-server
	   hd-cookie->port
	   hd-cookie->exit-proc
	   hd-cookie?
           wait-for-connection)

  (define start-help-server
    (opt-lambda ([use-port #f][external-connections? #f]) 
      (and (or (use-plt-browser?)
	       (get-preference 'external-browser 
			       (lambda () #f))
	       ;; should be no-op, except in Unix, where
	       ;; it gets a preference from the user
	       (update-browser-preference #f))
	   
	   (if (use-plt-browser?)
	       (internal-start-help-server)
	       (external-start-help-server use-port external-connections?))))))





	    
















