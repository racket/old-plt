(module browser mzscheme
  (require (lib "browser.ss" "net"))
  (require "server.ss")

  (provide help-desk-browser
	   help-desk-navigate)

  (define (help-desk-navigate url)
    (send-url url #t))

  (define (help-desk-browser hd-cookie)
    (help-desk-navigate 
     (format "http://127.0.0.1:~a/servlets/home.ss" 
	     (hd-cookie->port hd-cookie)))))
