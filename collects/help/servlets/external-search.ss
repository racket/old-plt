(module external-search mzscheme
  (require (lib "servlet-sig.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server"))
  
  (require "private/util.ss")
  (require "private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (define stupid-internal-define-syntax (report-errors-to-browser send/finish))
    
    (define (make-results-url 
             search-string search-type match-type lucky?)
      (format  
       (string-append 
        "/servlets/results.ss?"
        "search-string=~a&"
        "search-type=~a&"
        "match-type=~a&"
        "lucky=~a")
       (hexify-string search-string)
       search-type
       match-type
       lucky?))
    
    ; two ways to get here
    ; - by a search from an external program (like DrScheme), using
    ;    search-for-docs
    ; - by a manual search from an external program, using 
    ;    goto-manual-link
    ; if frames enabled, bottom frame shows search results
    
    (let* ([bindings (request-bindings initial-request)]
           [get-binding (lambda (sym)
                          (with-handlers
                              ([void (lambda _ #f)]) 
                            (extract-binding/single sym bindings)))]
           [search-string (get-binding 'search-string)]
           [search-type (get-binding 'search-type)]
           [match-type (get-binding 'match-type)]
           [lucky? (get-binding 'lucky)]
           [hd-url (get-binding 'hd-url)])
      (redirect-to
       (if hd-url
           ;; from goto-manual-link
           hd-url
           ;; from search-for-docs
           (make-results-url 
            search-string search-type match-type lucky?))))))