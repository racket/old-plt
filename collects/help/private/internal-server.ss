(module internal-server mzscheme
  (require (lib "etc.ss")
	   (lib "file.ss")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (require (lib "internal-server.ss" "web-server")
	   (lib "browser.ss" "net"))

  (require "server-config.ss")

  (require (lib "external.ss" "help" "servlets" "private"))

  (provide internal-start-help-server)

  ; ordinarily, we just build a fresh hd-cookie
  ; if one is passed in, as when the external browser timeout expires,
  ;  we imperatively update the passed-in cookie
  (define internal-start-help-server
    (opt-lambda ([hd-cookie #f])
      (install-help-browser-preference-panel) ; in case we need prefs
      (let*-values ([(configuration)
		     (build-developer-configuration
		      (build-config-exp))]
		    [(exit-proc browser-maker)
		     (serve configuration 
			    (or (and hd-cookie (hd-cookie->port hd-cookie))
				min-port))])
	(if hd-cookie
	    (begin
	      (set-hd-cookie-exit-proc! hd-cookie exit-proc)
	      (set-hd-cookie-browser! hd-cookie browser-maker)
	      hd-cookie)
	    (make-hd-cookie min-port exit-proc browser-maker))))))












