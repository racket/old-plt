(require-library "html.ss" "html")
(require-library "url.ss" "net")

(define pcdata-url (string->url "file:///home/bonfield/plt/collects/browser2/pcdata.html"))
(define a-html (call/input-url pcdata-url get-pure-port html:read-html))
(define html-contents (html:html-full-content a-html))  ; head and body
(define head-contents (map html:html-full-content
                           (filter (lambda (x)
                                     (html:head? x)) html-contents)))
(define body-contents (map html:html-full-content
                           (filter (lambda (x)
                                     (html:body? x)) html-contents)))
