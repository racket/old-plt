(module external-search mzscheme
  (require "browser.ss"
	   "server.ss"
	   (lib "util.ss" "help" "servlets" "private")
	   (lib "specs.ss" "framework"))

  (provide/contract 
   (search-for-docs
    (hd-cookie? string? 
		(lambda (s) 
		  (member s
		   '("keyword" "keyword-index" "keyword-index-text")))
		(lambda (s) 
		  (member s
		   '("exact-match" "containing-match" "regexp-match")))
		any?
		. -> . any?)))

  ; hd-cookie string string string any -> void
  ; shows search result in default browser
  (define (search-for-docs cookie search-string search-type match-type lucky?)
    (let* ([port (hd-cookie->port cookie)]
	   [url (format 
		 (string-append "http://127.0.0.1:~a/servlets/index.ss?"
				"search-string=~a&"
				"search-type=~a&"
				"match-type=~a&"
				"lucky=~a")
		 port (hexify-string search-string) search-type match-type
		 (if lucky? "true" "false"))])
      (help-desk-navigate url))))
