(require-library "browser.ss" "browser2")
(load "render-html.ss")
(load "render-table.ss")
(load "utils.ss")
(load "comma-delimited-text-parser.ss")

; Each HTML file represents a set of TEST CASES.
(define ListofURLs
  (list (string->url "http://www.cs.utah.edu/plt/develop/")
        ;(string->url "file:///home/bonfield/testcases/render-html/basic-renderers.html")
        ;(string->url "file:///home/bonfield/testcases/render-html/fontfacestest.html")
        ;(string->url "file:///home/bonfield/testcases/render-html/hr.html")
        ;(string->url "file:///home/bonfield/testcases/render-html/a.html")
        ;(string->url "file:///home/bonfield/testcases/render-html/img.html")
        ))

; This file contains a text description of each test case as
; it is expected to be rendered:
(define desc-filename "render-html-desc.txt")

(define path "/home/bonfield/testcases/render-html/")

; Reads character from the current input port and returns the list
; of characters after it reaches eof.
(define (thunk)
  (local [(define next-obj (read-char))]
    (cond [(eof-object? next-obj) empty]
          [else (cons next-obj (thunk))])))

; desc-filename as a printable string.
(define desc-text (list->string 
                   (with-input-from-file 
                       (string-append path desc-filename) thunk)))
  
; List of struct:html for each file in ListofFiles.
(define html-files 
  (map (lambda (a-url)
         (call/input-url a-url get-pure-port html:read-html))
       ListofURLs))

; test-suite : (listof (struct:html)) -> (void)
; For each html in the ListofHtml, renders the structure in its own frame.
(define (test-suite ListofHtml)
  (loop-until (sub1 (length ListofHtml))
              (lambda (x) (< x 0))
              sub1
              (lambda (i)
                (local [(define a-html (list-ref ListofHtml i))
                        (define frame (make-object frame% (string-append "OUTPUT for " (url->string (list-ref ListofURLs i))) #f 800 800))
                        (define text (make-object text%))
                        (define editor-canvas (make-object editor-canvas% frame text))]
                  (send text set-styles-sticky #f)
                  (send text auto-wrap #t)
                  (send frame show #t)
                  (send text begin-edit-sequence #f)
                  (render-html-page text (list-ref ListofURLs i) a-html)
                  (send text end-edit-sequence)))))

; render : text% -> (void)
; Called to render all test cases for render-html on a-text.
(define (render a-text)
  (send a-text insert desc-text)
  (test-suite html-files))

(render (make-object text%))