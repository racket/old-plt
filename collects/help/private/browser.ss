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
	   help-desk-navigate
           in-help-desk-navigate?)

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

  (define (prompt-for-browser-switch hd-cookie exn)
    (eq? 1 
	 (message-box/custom "PLT Help Desk error"
           (fold-string			
	    `(,(if exn
		   (fold-string
		    `("When starting a Web browser to connect to"
		      "the Help Desk server, an exception was"
		      "raised:"
		      ,(if (exn? exn)
			   (format "  ~a" (exn-message exn))
			   (format "  ~s" exn))))
		   (fold-string
		    `("Help Desk tried to use your Web browser, but"
		      "the browser has not made a connection to the"
		      "the Help Desk server.")))
	      ""
	      "Help Desk can also use a built-in browser."
	      "Would you like to use it instead?  You can"
	      "switch back to your own Web browser by "
	      "changing DrScheme's browser preference."
	      ""
	      "If you continue with an external browser, "
	      "the Help Desk home page may be found at"
	      ,(format (string-append home-page-format ".")
		       (hd-cookie->port hd-cookie))))
	   "Switch to built-in browser"     ; button 1
	   "Continue with external browser" ; button 2
	   #f                               ; button 3
	   #f                               ; parent
	   '(default=1)
	   2)))

  (define nav-mutex (make-semaphore 1))

  (define navigate? #f)
  (define (in-help-desk-navigate?)
    navigate?)

  (define debug-frame-class%
    (class frame%
	   (inherit show)
	   (field
	    [panel #f])
	   (public add-message)
	   (define (shutdown-proc)
	     (send this show #f))
	   (define/override can-close?  
	     (lambda () (shutdown-proc) #f))
	   (define (add-message s)
	     (instantiate message% ()
			  (label s)
			  (parent panel)))
	   (super-instantiate ())
	   (set! panel
		 (instantiate vertical-panel% ()
			      (parent this)
			      (alignment '(left center))))))

  (define (get-debug-frame)
    (instantiate debug-frame-class% ()
		 (label "Help Desk connection")
		 (min-width 225)
		 (stretchable-width #f)
		 (stretchable-height #f)))


  (define (start-help-desk-browser url hd-cookie)
    (let* ([mk-browser (hd-cookie->browser hd-cookie)]
	   [browser-frame-maybe (send-help-desk-url mk-browser url)])
      (when (is-a? browser-frame-maybe frame%)
	    (let* ([browser-panel (send browser-frame-maybe get-hyper-panel)]
		   [browser-frame (send browser-panel get-parent)])
	      (send browser-frame set-label "PLT Help Desk")
	      (send browser-panel set-init-page 
		    (format home-page-format
			    (hd-cookie->port hd-cookie)))))))

  (define (help-desk-navigate hd-cookie url)
    (when (semaphore-try-wait? nav-mutex)
	  (set! navigate? #t)
	  (let* ([nav-sem (make-semaphore 0)]
		 [start-exn #f]
		 [debug? (get-preference 'plt:help-debug (lambda () '#f))]
		 [frame #f]	
		 [debug-msg void])
	       (when debug?
		     (set! frame (get-debug-frame))
		     (set! debug-msg (lambda (s) 
				       (send frame add-message s)))
		     (send frame show #t))
	       (letrec
		   ([timer browser-timeout]
		    [monitor-thread
		     (thread
		      (lambda ()
			(debug-msg "Starting browser-connect thread")
			(wait-start-semaphore)
			(debug-msg "Browser connected")
			(kill-thread timer-thread)
			(debug-msg "Killed timer thread")
			(semaphore-post nav-sem)))]
		    [timer-thread
		     (thread
		      (lambda ()
			(debug-msg "Starting timer thread")
			(let loop ([n 0])
			  (when (< n timer)
				(sleep 1)
				(loop (add1 n))))
			(debug-msg "Timer expired")
			(when (prompt-for-browser-switch hd-cookie start-exn)
			      (set-plt-browser!)
			      (debug-msg "Shutting down external server")
			      ((hd-cookie->exit-proc hd-cookie))
			      (debug-msg "Starting internal server")
			      (internal-start-help-server hd-cookie)
			      (debug-msg "Starting internal browser")
			      ; should never fail, so no handler
			      (start-help-desk-browser url hd-cookie))
			(kill-thread monitor-thread)
			(semaphore-post nav-sem)))])
		 (debug-msg "Starting Help Desk browser")
		 (with-handlers 
		  ([(lambda _ #t) (lambda (exn)
				    (debug-msg 
				     "Starting Help Desk browser raised an exception")
				    (set! start-exn exn)
				    (set! timer 0))])
		  (start-help-desk-browser (build-dispatch-url hd-cookie url)
					   hd-cookie))
		 (yield nav-sem)
		 (set! navigate? #f)
		 (semaphore-post nav-mutex)))))
  
  (define (help-desk-browser hd-cookie)
    (help-desk-navigate hd-cookie 
			(format home-page-format 
				(hd-cookie->port hd-cookie)))))







