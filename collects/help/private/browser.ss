(module browser mzscheme
  (require (lib "list.ss")
	   (lib "browser.ss" "net")
           (lib "util.ss" "help" "servlets" "private"))

  (require "server-config.ss")

  (provide help-desk-browser help-desk-navigate)

  (define (build-dispatch-url hd-cookie url)
    (format "http://127.0.0.1:~a/servlets/start.ss?url=~a"
	    (hd-cookie->port hd-cookie)
	    (hexify-string url)))

  (define home-page-format "http://127.0.0.1:~a/servlets/home.ss")

  (define nav-mutex (make-semaphore 1))

  (define (start-help-desk-browser url hd-cookie)
    (let ([mk-browser (hd-cookie->browser hd-cookie)])
      (send-help-desk-url mk-browser url)))

  (define (help-desk-navigate hd-cookie url)
    (when (semaphore-try-wait? nav-mutex)
      (start-help-desk-browser url hd-cookie)
      (semaphore-post nav-mutex)))

  (define (help-desk-browser hd-cookie)
    (help-desk-navigate hd-cookie 
			(format home-page-format 
				(hd-cookie->port hd-cookie)))))
