(module browser mzscheme
  (require (lib "file.ss")
	   (lib "browser.ss" "net")
           (lib "util.ss" "help" "servlets" "private"))

  (require "server-config.ss")
  (require "internal-server.ss")
  (require "start.ss")
  (require "plt-browser.ss")

  (provide help-desk-browser
	   help-desk-navigate)

  (define tried #f)
  (define (tried-external-browser?)
    tried)

  (define (build-dispatch-url hd-cookie url)
    (format "http://127.0.0.1:~a/servlets/start.ss?url=~a"
	    (hd-cookie->port hd-cookie)
	    (hexify-string url)))

  (define browser-param (external-browser))

  (define (help-desk-navigate hd-cookie url)
    (if (tried-external-browser?)
        ; starting internal server changes value of parameter
        ;  in timer-thread; its value is copied out to main thread as 
        ;  browser-param
        (parameterize
	 ([external-browser browser-param])
	 (send-url url #t))
	(letrec
	  ([monitor-thread
	    (thread
	     (lambda ()
	       (wait-start-semaphore)
	       (set! tried #t)
	       (kill-thread timer-thread)))]
	   [timer-thread
	    (thread
	     (lambda ()
	       (sleep browser-timeout)
	       (set! tried #t)
	       (kill-thread monitor-thread)
	       (set-plt-browser!)
               ; shutdown old server
	       ((hd-cookie->exit-proc hd-cookie))
	       (fprintf (current-error-port)
			"Switching to PLT browser.~n")
	       (internal-start-help-server hd-cookie)
               (set! browser-param (external-browser))
	       (send-url url)))])
          (with-handlers 
	    ([void (lambda _ (fprintf (current-error-port)
				      (string-append
				       "Help Desk browser failed.~n")))])
	    (send-url (build-dispatch-url hd-cookie url))))))

  (define (help-desk-browser hd-cookie)
    (help-desk-navigate hd-cookie 
			(format "http://127.0.0.1:~a/servlets/home.ss"
				(hd-cookie->port hd-cookie)))))









