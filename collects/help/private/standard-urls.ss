(module standard-urls mzscheme

  (require "../servlets/private/util.ss"
           "cookie.ss"
           "finddoc.ss"
	   (lib "contracts.ss"))
  
  (provide make-results-url
	   make-relative-results-url
           make-home-page-url
           prefix-with-server)
  
  (provide/contract 
   (make-missing-manual-url (hd-cookie? string? string? string? . -> . string?))
   (search-for-docs (hd-cookie? 
                     string? 
                     (lambda (s) 
                       (member s
                               '("keyword" "keyword-index" "keyword-index-text")))
                     (lambda (s) 
                       (member s
                               '("exact-match" "containing-match" "regexp-match")))
                     any?
                     . -> . any?))
   (goto-manual-link (hd-cookie? string? string? . -> . any?))
   (goto-hd-location (hd-cookie? (lambda (sym)
                                   (memq sym hd-location-syms))
                                 . -> . 
                                 any)))
  
  (define (prefix-with-server cookie suffix)
    (format "http://127.0.0.1:~a~a"
            (hd-cookie-port cookie)
            suffix))
  
  (define results-url-prefix
    "http://127.0.0.1:~a/servlets/results.ss?")
  
  (define relative-results-url-prefix "/servlets/results.ss?")

  (define (make-home-page-url port)
    (format "http://127.0.0.1:~a/servlets/home.ss" port))
  
  (define (make-missing-manual-url cookie coll name link)
    (format "http://127.0.0.1:~a/servlets/missing-manual.ss?manual=~a&name=~a&link=~a"
            (hd-cookie-port cookie)
            coll
            (hexify-string name)
            (hexify-string link)))
  
  (define (make-relative-results-url search-string search-type match-type lucky?)
    (format 
     (string-append relative-results-url-prefix
                    "search-string=~a&"
                    "search-type=~a&"
                    "match-type=~a&"
                    "lucky=~a")
     (hexify-string search-string)
     search-type
     match-type
     (if lucky? "true" "false")))

  (define (make-results-url port search-string search-type match-type lucky?)
    (format 
     (string-append results-url-prefix
                    "search-string=~a&"
                    "search-type=~a&"
                    "match-type=~a&"
                    "lucky=~a")
     port 
     (hexify-string search-string)
     search-type
     match-type
     (if lucky? "true" "false")))
  
  ; sym, string assoc list
  (define hd-locations
    '((hd-tour "/doc/tour/")
      (release-notes "/servlets/release/notes.ss")
      (plt-license "/servlets/release/license.ss")
      (front-page "/servlets/home.ss")))
  
  (define hd-location-syms (map car hd-locations))

  ; hd-cookie string string string any -> void
  ; shows search result in default browser
  (define (search-for-docs cookie search-string search-type match-type lucky?)
    (unless (string=? search-string "")
      (let* ([port (hd-cookie-port cookie)]
             [url (make-results-url (hd-cookie-port cookie)
                                    search-string
                                    search-type
                                    match-type
                                    lucky?)])
        (put-prefs '(plt:hd:search-type
                     plt:hd:match-type)
                   (list search-type match-type))
        (visit-url-in-browser cookie url))))

  (define (goto-manual-link cookie manual index-key)
    (let* ([hd-url (finddoc-page-anchor manual index-key)]
	   [url (prefix-with-server cookie hd-url)])
      (visit-url-in-browser cookie url)))

  (define (goto-hd-location cookie sym)
    ; the assq is guarded by the contract
    (let ([entry (assq sym hd-locations)])
      (visit-url-in-browser 
       cookie
       (prefix-with-server 
	cookie
	(cadr entry))))))