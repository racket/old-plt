(module server mzscheme 

  (require (lib "etc.ss")
	   (lib "file.ss")
	   (lib "util.ss" "web-server")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (require (prefix external: 
		   (lib "web-server.ss" "web-server"))
	   (prefix internal: 
		   (lib "internal-server.ss" "web-server")))

  (require "server-config.ss")
  (require (prefix external: "external-server.ss"))
  (require (prefix internal: "internal-server.ss"))

  (provide start-help-server
	   hd-cookie->port
	   hd-cookie->exit-proc
	   hd-cookie?
           (rename external:wait-for-connection wait-for-connection))

  (define start-help-server
    (opt-lambda ([use-port #f][external-connections? #f]) 
      (let ([browser-pref
	     (get-preference 'external-browser (lambda () #f))])
	(if (eq? browser-pref 'plt)
	    (internal:start-help-server)
	    (external:start-help-server use-port external-connections?))))))















