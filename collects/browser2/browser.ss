(require-library "refer.ss")
(require-library "macro.ss")
(require-library "sig.ss" "browser2")
(require-library "function.ss")
(require-library "file.ss")
(require-library "xml.ss" "xml")
(require-library "html.ss" "html")
(require-library "url.ss" "net")

(define-values/invoke-unit/sig browser2^
  (unit/sig browser2^
    (import mred^
            mzlib:file^
            mzlib:function^
            xml^
            (html : html^)
            mzlib:url^)

    (include "comma-delimited-text-parser.ss")
    (include "utils.ss")
    (include "render-html.ss")
    (include "render-table.ss")
    
    (define (browse-url a-url)
      (local [(define scheme (url-scheme a-url))
              (define fragment (url-fragment a-url))
              (define path (url-path a-url))
              (define file-suffix (get-file-suffix path))
              (define frame (make-object frame% (url->string a-url) #f 800 800))
              (define text (make-object text%))
              (define editor-canvas (make-object editor-canvas% frame text))]
        (send text set-styles-sticky #f)
        (send text auto-wrap #t)
        (send frame show #t)
        (send text begin-edit-sequence)
        (render-html-page text a-url (call/input-url a-url get-pure-port html:read-html))
        (send text end-edit-sequence))))
  #f
  mred^
  mzlib:file^
  mzlib:function^
  xml^
  (html : html^)
  mzlib:url^)

(define plt-doc-url (string->url "file:///home/bonfield/plt/collects/doc/drscheme/index.html"))
(define utah-plt-url (string->url "file:///home/bonfield/plt/collects/browser2/utah-plt-doc.html"))
(define ol-url (string->url "file:///home/bonfield/plt/collects/browser2/test.html"))
(define basics-url (string->url "file:///home/bonfield/plt/collects/browser2/testcases/render-html/basic-renderers.html"))
(define a-html (call/input-url utah-plt-url get-pure-port html:read-html))
(define html-contents (html:html-full-content a-html))  ; head and body
(define head-contents (map html:html-full-content
                           (filter (lambda (x)
                                     (html:head? x)) html-contents)))
(define body-contents (map html:html-full-content
                           (filter (lambda (x)
                                     (html:body? x)) html-contents)))
