(module internal-server mzscheme
  (require (lib "etc.ss")
	   (lib "file.ss")
	   (lib "configuration.ss" "web-server")
	   (lib "configuration-structures.ss" "web-server"))

  (require (lib "internal-server.ss" "web-server"))

  (require "server-config.ss")

  (require (lib "external.ss" "help" "servlets" "private"))

  (provide internal-start-help-server)

  (define (internal-start-help-server)
    (let ([configuration
	   (build-developer-configuration
	    (build-config-exp))])
      (make-hd-cookie min-port (serve configuration min-port)))))











