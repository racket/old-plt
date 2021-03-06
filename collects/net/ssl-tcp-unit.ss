(module ssl-tcp-unit mzscheme
  (provide make-ssl-tcp@)
  (require (lib "unitsig.ss")
           "tcp-sig.ss"
	   (lib "mzssl.ss" "openssl")
	   (lib "etc.ss"))
  
  (define (make-ssl-tcp@ 
	   server-cert-file server-key-file server-root-cert-files server-suggest-auth-file
	   client-cert-file client-key-file client-root-cert-files)
    (unit/sig net:tcp^
      (import)

      (define ctx (ssl-make-client-context))
      (when client-cert-file
	(ssl-load-certificate-chain! ctx client-cert-file))
      (when client-key-file
	(ssl-load-private-key! ctx client-key-file))
      (when client-root-cert-files
	(ssl-set-verify! ctx #t)
	(map (lambda (f)
	       (ssl-load-verify-root-certificates! ctx f))
	     client-root-cert-files))

      (define (tcp-abandon-port p)
	(if (input-port? p)
	    (close-input-port p)
	    (close-output-port p)))

      (define tcp-accept ssl-accept)

      ;; accept-ready? doesn't really work for SSL:
      (define (tcp-accept-ready? p)
	#f)

      (define tcp-addresses ssl-addresses)
      (define tcp-close ssl-close)
      (define tcp-connect 
	(opt-lambda (hostname port-k)
	  (ssl-connect hostname port-k ctx)))
      (define tcp-connect/enable-break
	(opt-lambda (hostname port-k)
	  (ssl-connect/enable-break hostname port-k ctx)))

      (define tcp-listen
	(opt-lambda (port [allow-k 4] [reuse? #f] [hostname #f])
	  (let ([l (ssl-listen port allow-k reuse? hostname)])
	    (when server-cert-file
	      (ssl-load-certificate-chain! l server-cert-file))
	    (when server-key-file
	      (ssl-load-private-key! l server-key-file))
	    (when server-root-cert-files
	      (ssl-set-verify! l #t)
	      (map (lambda (f)
		     (ssl-load-verify-root-certificates! l f))
		   server-root-cert-files))
	    (when server-suggest-auth-file
	      (ssl-load-suggested-certificate-authorities! l server-suggest-auth-file))
	    l)))

      (define tcp-listener? ssl-listener?))))
