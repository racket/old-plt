;; copyright by Paul Graunke June 2000 AD
(require-library "sgml.ss" "html")
(require-library "function.ss")
(load-relative "html-structs.ss")
(load-relative "case.ss")

;; xml->html : Document -> Html
(define (xml->html doc)
  (let ([root (document-element doc)])
    (unless (eq? 'html (element-name root))
      (error 'xml->html "This is not an html document.  Expected 'html, given ~a" (element-name root)))
    (make-html (element-attributes root) (xml-contents->html (element-content root)))))

;; xml-content->html : (listof Content) -> Html-element
(define (xml-contents->html contents)
  (foldr xml-single-content->html
         null
         contents))

;; read-xhtml : [Input-port] -> Html-element
(define read-xhtml (compose xml->html read-xml))

;; read-html : [Input-port] -> Html-element
(define read-html
  (compose xml->html (sgml:gen-read-sgml (sgml:gen-may-contain (call-with-input-file "html-spec" read)))))
