(module standard-urls mzscheme

  (require "../servlets/private/util.ss"
           "cookie.ss"
	   (lib "contract.ss"))
  
  (provide make-home-page-url
           prefix-with-server)
  
  (define (search-type? x)
    (member x '("keyword" "keyword-index" "keyword-index-text")))
  
  (define (search-how? x)
    (member x '("exact-match" "containing-match" "regexp-match")))
  
  (provide/contract 
   (make-relative-results-url (string? search-type? search-how? any? (listof symbol?) any?
                                       (union false? string?) . -> . string?))
   (make-results-url (number? 
                      string?
                      search-type? search-how? any? 
                      (listof symbol?) any? (union false? string?) . -> . string?))
   (make-missing-manual-url (hd-cookie? string? string? string? . -> . string?))
   (search-for-docs (hd-cookie? string? search-type? search-how? any? (listof symbol?) any? 
                                (union false? string?) . -> . any?))
   (goto-hd-location (hd-cookie? (lambda (sym)
                                   (memq sym hd-location-syms))
                                 . -> . 
                                 any))
   [make-docs-plt-url (string? . -> . string?)]
   [make-docs-html-url (string? . -> . string?)])

  (define (make-docs-plt-url manual-name)
    (format "http://download.plt-scheme.org/doc/~a/bundles/~a-doc.plt"
            (if (cvs-or-nightly-build?)
                "pre-release"
                (version))
            manual-name))
  
  (define (make-docs-html-url manual-name)
    (format "http://download.plt-scheme.org/doc/~a/html/~a/index.htm" 
            (if (cvs-or-nightly-build?)
                "pre-release"
                (version))
            manual-name))
  
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
  
  (define (make-relative-results-url search-string search-type match-type lucky? manuals doc.txt? lang-name)
    (string-append
     relative-results-url-prefix
     (make-results-url-args search-string search-type match-type lucky? manuals doc.txt? lang-name)))

  (define (make-results-url port search-string search-type match-type lucky? manuals doc.txt? lang-name)
    (string-append
     (format results-url-prefix port)
     (make-results-url-args search-string search-type match-type lucky? manuals doc.txt? lang-name)))
  
  (define (make-results-url-args search-string search-type match-type lucky? manuals doc.txt? language-name)
    (let ([start
           (format
            (string-append "search-string=~a&"
                           "search-type=~a&"
                           "match-type=~a&"
                           "lucky=~a&"
                           "manuals=~a&"
                           "doctxt=~a")
            (hexify-string search-string)
            search-type
            match-type
            (if lucky? "true" "false")
            (hexify-string (format "~s" manuals))
            (if doc.txt? "true" "false"))])
      (if language-name
          (string-append start (format "&langname=~a" (hexify-string language-name)))
          start)))
  
  ; sym, string assoc list
  (define hd-locations
    '((hd-tour "/doc/tour/")
      (release-notes "/servlets/release/notes.ss")
      (plt-license "/servlets/release/license.ss")
      (front-page "/servlets/home.ss")))
  
  (define hd-location-syms (map car hd-locations))

  (define (search-for-docs cookie search-string search-type match-type lucky? manuals doc.txt? lang-name)
    (unless (string=? search-string "")
      (let* ([port (hd-cookie-port cookie)]
             [url (make-results-url (hd-cookie-port cookie)
                                    search-string
                                    search-type
                                    match-type
                                    lucky?
                                    manuals 
                                    doc.txt?
                                    lang-name)])
        (visit-url-in-browser cookie url))))

  (define (goto-hd-location cookie sym)
    ; the assq is guarded by the contract
    (let ([entry (assq sym hd-locations)])
      (visit-url-in-browser 
       cookie
       (prefix-with-server 
	cookie
	(cadr entry))))))