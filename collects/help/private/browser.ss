(module browser mzscheme
  (require (lib "class.ss")
	   (lib "file.ss")
	   (lib "list.ss")
	   (lib "mred.ss" "mred")
	   (lib "browser.ss" "net")
           (lib "util.ss" "help" "servlets" "private"))

  (require "server-config.ss")
  (require "internal-server.ss")
  (require "start.ss")
  (require "plt-browser.ss")

  (provide help-desk-browser
	   help-desk-navigate)

  (define (build-dispatch-url hd-cookie url)
    (format "http://127.0.0.1:~a/servlets/start.ss?url=~a"
	    (hd-cookie->port hd-cookie)
	    (hexify-string url)))

  (define home-page-format "http://127.0.0.1:~a/servlets/home.ss")

  (define (fold-string ss)
    (foldr (lambda (s a)
	     (if a
		 (string-append s nl a)
		 s))
	   #f
	   ss))

  (define (prompt-for-browser-switch hd-cookie)
    (eq? 1 
	 (message-box/custom "Help Desk timeout"
           (fold-string			
	    `("Help Desk tried to use your Web browser, but"
	      "the browser has not made a connection to the"
	      "the Help Desk server."
	      ""
	      "Help Desk can also use a built-in browser."
	      "Would you like to use it instead?  You can"
	      "switch back to your own Web browser by "
	      "changing DrScheme's browser preference."
	      ""
	      "If you continue with an external browser, "
	      "the Help Desk home page may be found at"
	      ,(format home-page-format 
		       (hd-cookie->port hd-cookie))))
	   "Switch to built-in browser"     ; button 1
	   "Continue with external browser" ; button 2
	   #f                               ; button 3
	   #f                               ; parent
	   '(default=1)
	   2)))

  (define nav-mutex (make-semaphore 1))
  
  (define (help-desk-navigate hd-cookie url)
    (semaphore-wait nav-mutex)
    (let ([nav-sem (make-semaphore 0)])
      (letrec
	  ([monitor-thread
	    (thread
	     (lambda ()
	       (wait-start-semaphore)
	       (kill-thread timer-thread)
	       (semaphore-post nav-sem)))]
	   [timer-thread
	    (thread
	     (lambda ()
	       (sleep browser-timeout)
	       (when (prompt-for-browser-switch hd-cookie)
		     (set-plt-browser!)
		     ; shutdown old server
		     ((hd-cookie->exit-proc hd-cookie))
		     (internal-start-help-server hd-cookie)
		     (send-help-desk-url (hd-cookie->browser hd-cookie) url))
	       (kill-thread monitor-thread)
	       (semaphore-post nav-sem)))])
	(with-handlers 
	 ([void (lambda _ (fprintf (current-error-port)
				   (string-append
				    "Help Desk browser failed.~n")))])
	 (send-help-desk-url (hd-cookie->browser hd-cookie) 
			     (build-dispatch-url hd-cookie url))
	 (yield nav-sem)
	 (semaphore-post nav-mutex)))))
  
  (define (help-desk-browser hd-cookie)
    (help-desk-navigate hd-cookie 
			(format home-page-format 
				(hd-cookie->port hd-cookie)))))






