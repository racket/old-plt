(module server mzscheme 

  (require (lib "etc.ss"))

  (require "browser.ss"
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
      (if (use-plt-browser?)
	  (internal-start-help-server)
	  (external-start-help-server use-port external-connections?)))))
















