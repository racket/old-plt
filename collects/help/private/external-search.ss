(module external-search mzscheme
  (require "browser.ss"
	   "server-config.ss"
	   "finddoc.ss"
	   (lib "contracts.ss")
	   (lib "util.ss" "help" "servlets" "private"))

  ; sym, string assoc list
  (define hd-locations
    '((hd-tour "/doc/tour/")
      (release-notes "/servlets/release/notes.ss")
      (plt-license "/servlets/release/license.ss")))
  (define hd-location-syms
    (map car hd-locations))

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
    (hd-cookie? string? string? . -> . any?))
   (goto-hd-location
    (hd-cookie? (lambda (sym)
		  (memq sym hd-location-syms))
		. -> . any?)))

  (define search-url-prefix
    "http://127.0.0.1:~a/servlets/external-search.ss?")

  ; hd-cookie string string string any -> void
  ; shows search result in default browser
  (define (search-for-docs cookie search-string search-type match-type lucky?)
    (if (string=? search-string "")
	(help-desk-browser cookie)
	(let* ([port (hd-cookie->port cookie)]
	       [url (format 
		     (string-append search-url-prefix
				    "search-string=~a&"
				    "search-type=~a&"
				    "match-type=~a&"
				    "lucky=~a")
		     port (hexify-string search-string) search-type match-type
		     (if lucky? "true" "false"))])
	  (put-prefs '(plt:hd:search-type
		       plt:hd:match-type)
		     (list search-type match-type))
	  (help-desk-navigate cookie url))))

  ; cookie is an hd-cookie struct
  ; hd-url is /doc/<manual>/... or /servlet/...
  (define (make-external-search-url cookie hd-url) 
    (format
     (string-append search-url-prefix "hd-url=~a")
     (hd-cookie->port cookie)
     (hexify-string hd-url)))

  (define (goto-manual-link cookie manual index-key)
    (let* ([port (hd-cookie->port cookie)]
	   [hd-url (finddoc-page-anchor manual index-key)]
	   [url (make-external-search-url cookie hd-url)])
      (help-desk-navigate cookie url)))

  (define (goto-hd-location cookie sym)
    ; the assq is guarded by the contract
    (let ([entry (assq sym hd-locations)])
      (help-desk-navigate 
       cookie
       (make-external-search-url 
	cookie
	(cadr entry))))))


	   
    
