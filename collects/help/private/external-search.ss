(module external-search mzscheme
  (require "browser.ss"
	   "server.ss"
	   "finddoc.ss"
	   (lib "contracts.ss")
	   (lib "util.ss" "help" "servlets" "private"))

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
		. -> . any?))
   (goto-manual-link
    (hd-cookie? string? string? . -> . any?)))

  (define (index-format-prefix)
    (if (use-frames?)
	"http://127.0.0.1:~a/servlets/index.ss?"
	"http://127.0.0.1:~a/servlets/results.ss?"))

  ; hd-cookie string string string any -> void
  ; shows search result in default browser
  (define (search-for-docs cookie search-string search-type match-type lucky?)
    (let* ([port (hd-cookie->port cookie)]
	   [url (format 
		 (string-append (index-format-prefix)
				"search-string=~a&"
				"search-type=~a&"
				"match-type=~a&"
				"lucky=~a")
		 port (hexify-string search-string) search-type match-type
		 (if lucky? "true" "false"))])
      (help-desk-navigate url)))

  (define (goto-manual-link cookie manual index-key)
    (let* ([port (hd-cookie->port cookie)]
	   [hd-url (finddoc-page-anchor manual index-key)]
           ; hd-url is /doc/<manual>/... or /servlet/...
	   [url (format
		 (string-append (index-format-prefix)
				"hd-url=~a")
		 (hd-cookie->port cookie)
		 (hexify-string hd-url))])
      (help-desk-navigate url))))


	   
    
