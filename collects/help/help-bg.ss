(module help-bg mzscheme 
  (require (lib "cmdline.ss")
           "private/server-config.ss"
	   "private/external-server.ss")

  (define remote-connections? #f)
  (define quiet? #f)
  (define port #f)

  (command-line
   "background-help-desk"
   (current-command-line-arguments)
   (once-each
    [("-r" "--remote-connections") "Allow remote connections"
     (set! remote-connections? #t)]
    [("-q" "--quiet") "Don't print port information"
     (set! quiet? #t)]
    [("-p" "--port") number "Use given port number"
     (with-handlers
      ((void (lambda _
	       (error "Help Desk: expected exact integer for port"))))
      (let ([port-val (string->number number)])
	(unless (and (integer? port-val) (exact? port-val))
		(raise 'not-exact-integer))
	(set! port port-val)))]))

  (define hd-cookie (external-start-help-server port remote-connections? #f))
  (define help-desk-port (hd-cookie->port hd-cookie))

  ; allow server startup time
  (wait-for-connection help-desk-port)

  (unless quiet?
    (printf "Help Desk server is running on port ~a~n"
	    (hd-cookie->port hd-cookie)))

  (semaphore-wait (make-semaphore 0)))



	
	
	
	
	
	
	
	
	
	
