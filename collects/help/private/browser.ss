(module browser mzscheme
  (require (lib "browser.ss" "net")
	   (lib "util.ss" "help" "servlets" "private"))
  (require "server.ss")

  (provide help-desk-browser
	   help-desk-navigate)

  (define (help-desk-navigate url)
    (send-url url (get-pref/default 'new-browser new-browser-default)))

  (define (help-desk-browser hd-cookie)
    (help-desk-navigate 
     (format "http://127.0.0.1:~a/servlets/home.ss" 
	     (hd-cookie->port hd-cookie)))))
