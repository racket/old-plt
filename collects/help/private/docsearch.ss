(module docsearch mzscheme
  (provide search-for-docs)

  (require (lib "sendurl.ss" "net")
	   (lib "server.ss" "help")
  	   (lib "util.ss" "doc" "help" "servlets" "private"))

  ; hd-cookie string sym sym -> void
  ; shows search result in default browser
  (define (search-for-docs cookie search-string search-type match-type)
    (let* ([port (hd-cookie->port cookie)]
	   [url (format 
		 (string-append "http://127.0.0.1:~a/servlets/index.ss?"
				"search-string=~a&search-type=~a&"
				"match-type=~a")
		 port (hexify-string search-string) search-type match-type)])
      (if (eq? (system-type) 'windows)
	  (shell-execute #f url "open" (current-directory) 'SW_SHOWNORMAL)
	  (send-url url #t)))))

