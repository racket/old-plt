(module browser mzscheme
  (require (lib "browser.ss" "net"))
  (require "server.ss")

  (provide help-desk-browser
	   help-desk-navigate)

  (define (help-desk-navigate url)
    (if (eq? (system-type) 'windows)
	(shell-execute #f url "" (current-directory) 'SW_SHOWNORMAL)
	(send-url url #f)))

  (define (help-desk-browser hd-cookie)
    (help-desk-navigate 
     (format "http://127.0.0.1:~a/servlets/home.ss" 
	     (hd-cookie->port hd-cookie)))))
