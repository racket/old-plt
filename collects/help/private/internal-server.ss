(module internal-server mzscheme
  (require (lib "etc.ss")
	   (lib "file.ss")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (require (lib "internal-server.ss" "web-server"))

  (require "server-config.ss")

  (require (lib "external.ss" "help" "servlets" "private"))

  (provide internal-start-help-server)

  ; ordinarily, we just build a fresh hd-cookie
  ; if one is passed in, as when the external browser timeout expires,
  ;  we imperatively update the passed-in cookie
  (define internal-start-help-server
    (opt-lambda ([hd-cookie #f])
      (let* ([configuration
	      (build-developer-configuration
	       (build-config-exp))]
	     [exit-proc (serve configuration min-port)])
	(if hd-cookie
	    (begin
	      (set-hd-cookie-port! hd-cookie min-port)
	      (set-hd-cookie-exit-proc! hd-cookie exit-proc)
	      hd-cookie)
	    (make-hd-cookie min-port exit-proc))))))












