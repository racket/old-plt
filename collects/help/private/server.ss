(module server mzscheme 

  (require (lib "etc.ss")
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
      (let-values
       ([(starter param-getter)
	 (if (use-plt-browser?)
	     (values internal-start-help-server
		     external-browser)
	     (values (lambda () 
		       (external-start-help-server
			use-port external-connections?))
		     external-browser-preference))])
       (let ([exit-proc (starter)])
	 (set-browser-param! (param-getter))
	 exit-proc)))))




	    
















