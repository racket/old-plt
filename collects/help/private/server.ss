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
           hd-cookie->browser-mixin
	   hd-cookie?
           wait-for-connection)
  
  (define start-help-server
    (opt-lambda ([use-port #f]
                 [remote-connections? #f] 
                 [addl-browser-frame-mixin (lambda (x) x)])
      (if (use-plt-browser?)
          (internal-start-help-server addl-browser-frame-mixin)
          (external-start-help-server use-port
                                      remote-connections?
                                      addl-browser-frame-mixin)))))
