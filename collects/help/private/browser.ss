(module browser mzscheme
  (require (lib "class.ss")
	   (lib "file.ss")
	   (lib "mred.ss" "mred")
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

  (define home-page-format "http://127.0.0.1:~a/servlets/home.ss")

  (define prompt-frame #f)

  (define (prompt-for-browser-switch hd-cookie)
    (let* ([dialog-sem (make-semaphore 0)]
	   [prompt-value #f]
	   [hd-frame%
	    (class frame%
		   (inherit show)
		   (field
		    [panel #f])
		   (define (shutdown-proc)
		     (send this show #f)
		     (semaphore-post dialog-sem))
		   (define/override can-close?  
		     (lambda () (shutdown-proc) #f))
		   (super-instantiate ())
		   (set! panel
			 (instantiate vertical-panel% ()
				      (parent this)))
		   (for-each
		    (lambda (s)
		      (instantiate message% ()
				   (label s)
				   (parent panel)))
		    `("The Help Desk server has not received"
		      "a communication from a browser within"
		      "the allotted time."
		      ""
		      "If a browser still has not started,"
		      "you can manually start a browser with the URL"
		      ,(format home-page-format (hd-cookie->port hd-cookie))
		      "or switch to the PLT browser."))
		   (instantiate button% ()
				(label "Switch to PLT browser")
				(parent panel)
				(min-width 100)
				(callback 
				 (lambda (b ev)
				   (set! prompt-value #t)
				   (send this show #f)
				   (semaphore-post dialog-sem))))
		   (instantiate button% ()
				(label "Do not switch")
				(parent panel)
				(min-width 100)
				(callback (lambda (b ev) (shutdown-proc)))))]
	   [frame 
	    (instantiate hd-frame% ()
			 (label "Help Desk timeout")
			 (min-width 225)
			 (stretchable-width #f)
			 (stretchable-height #f))])
      (set! prompt-frame frame)
      (send frame center)
      (send frame show #t)
      (yield dialog-sem)
      prompt-value))
  
  (define nav-mutex (make-semaphore 1))
  
  (define (help-desk-navigate hd-cookie url)
    (semaphore-wait nav-mutex)
    (let ([nav-sem (make-semaphore 0)])
      (if (tried-external-browser?)
	(parameterize
	   ([external-browser (get-browser-param)])
	   (send-url url #t)
           (semaphore-post nav-mutex))
	  (begin
            ; should be no-op, except in Unix
	    (unless (or (eq? (help-browser-preference) 'plt)
			(get-preference 'external-browser 
					(lambda () #f)))
		    (update-browser-preference url))
	    (letrec
		([monitor-thread
		  (thread
		   (lambda ()
		     (wait-start-semaphore)
		     (kill-thread timer-thread)
		     (when prompt-frame
                       (send prompt-frame show #f))
		     (set! tried #t)
		     (semaphore-post nav-sem)))]
		 [timer-thread
		  (thread
		   (lambda ()
		     (sleep browser-timeout)
		     (set! tried #t)
		     (when (prompt-for-browser-switch hd-cookie)
			   (set-plt-browser!)
			   ; shutdown old server
			   ((hd-cookie->exit-proc hd-cookie))
			   (internal-start-help-server hd-cookie)
			   (send-url url))
		     (kill-thread monitor-thread)
		     (semaphore-post nav-sem)))])
	      (with-handlers 
	       ([void (lambda _ (fprintf (current-error-port)
					 (string-append
					  "Help Desk browser failed.~n")))])
	       (send-url (build-dispatch-url hd-cookie url))
	       (yield nav-sem)
	       (semaphore-post nav-mutex)))))))
  
  (define (help-desk-browser hd-cookie)
    (help-desk-navigate hd-cookie 
			(format home-page-format 
				(hd-cookie->port hd-cookie)))))






