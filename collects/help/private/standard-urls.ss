(module standard-urls mzscheme

  (require (lib "util.ss" "help" "servlets" "private"))
  
  (provide make-search-for-docs-url
           make-results-url
           make-external-search-url
           make-home-page-url)
  
  (define search-url-prefix
    "http://127.0.0.1:~a/servlets/external-search.ss?")
  
  (define results-url-prefix
    "http://127.0.0.1:~a/servlets/results.ss?")
  
  (define (make-home-page-url port)
    (format "http://127.0.0.1:~a/servlets/home.ss" port))
  
  (define (make-search-for-docs-url port search-string search-type match-type lucky?)
    (format 
     (string-append search-url-prefix
                    "search-string=~a&"
                    "search-type=~a&"
                    "match-type=~a&"
                    "lucky=~a")
     port 
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
  
  ; cookie is an hd-cookie struct
  ; hd-url is /doc/<manual>/... or /servlet/...
  (define (make-external-search-url port hd-url) 
    (format
     (string-append search-url-prefix "hd-url=~a")
     port
     (hexify-string hd-url))))