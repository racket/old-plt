;; copyright by Paul Graunke June 2000 AD
(unit/sig html^
  (import xml^ (sgml : sgml-reader^) mzlib:function^ mzlib:file^)
  
  ;; Html-content = Html-element | Pc-data | Entity   
  
  (include "html-structs.ss")
  (include "case.ss")
  
  ;; xml->html : Document -> Html
  (define (xml->html doc)
    (let ([root (document-element doc)])
      (unless (eq? 'html (element-name root))
        (error 'xml->html "This is not an html document.  Expected 'html, given ~a" (element-name root)))
      (make-html (element-attributes root) (xml-contents->html (element-content root)))))
  
  
  ;; xml-content->html : (listof Content) -> (listof Html-element)
  (define (xml-contents->html contents)
    (foldr xml-single-content->html
           null
           contents))
  
  ;; read-xhtml : [Input-port] -> Html
  (define read-xhtml (compose xml->html read-xml))
  
  ;; peel-f : (Html-content -> Bool) (listof Html-content) (listof Html-content) -> (listof Html-content)
  (define (peel-f toss? to-toss acc0)
    (foldr (lambda (x acc)
             (if (toss? x)
                 (append (html-full-content x) acc)
                 (cons x acc)))
           acc0
           to-toss))
  
  ;; repackage-html : (listof Html-content) -> Html
  (define (repackage-html contents)
    (let* ([html (memf html? contents)]
           [peeled (peel-f html? contents null)]
           [body (memf body? peeled)])
      (make-html (if html
                     (html-element-attributes (car html))
                     null)
                 (append (filter head? peeled)
                         (list (make-body (if body
                                              (html-element-attributes (car body))
                                              null)
                                          (filter (compose not head?) (peel-f body? peeled null))))))))
  
  ;; implicit-starts : Symbol Symbol -> (U #f Symbol)
  (define (implicit-starts parent child)
    (and (eq? child 'tr) (eq? parent 'table) 'tbody))
  
  ;; read-html : [Input-port] -> Html
  (define read-html
    (compose repackage-html xml-contents->html (sgml:gen-read-sgml (sgml:gen-may-contain (call-with-input-file (find-library "html-spec" "html") read)) implicit-starts))))
