(require-library "browser.ss" "browser2")

; Each HTML file represents a set of TEST CASES.
(define ListofFiles
  (list ;"case1.html"  ;implicit table definitions only
        ;"case2.html"  ;explicit column definitions only
        ;"case3.html"  ;explicit column definitions coincide with implicit column defs.
        ;"case4.html"  ;(< number-of-explicit-column-definitions number-of-implicit-column-defs)
        "case5.html"  ; table inserted in the middle of line of text (showing no reflowing)
                      ; uses cellpadding and cellspacing, rules, and width
        ;"case6.html"  
        ;"case7.html"  ;major error case: two merged cells overlap.
        ;"case8.html"  ;contains a caption ** not functional yet
        ))

; This file contains a text description of each test case as
; it is expected to be rendered:
(define desc-filename "render-table-desc.txt")

; The path to the ListofFiles.
(define path "./testcases/render-table/")

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
  (map (lambda (file)
         (call-with-input-file file html:read-html))
       (map (lambda (x)
              (string-append path x))
            ListofFiles)))

; test-suite : (listof (struct:html)) -> (void)
; For each html in the ListofHtml, renders the structure in its own frame.
(define (test-suite ListofHtml)
  (loop-until (sub1 (length ListofHtml))
              (lambda (x) (< x 0))
              sub1
              (lambda (i)
                (local [(define a-html (list-ref ListofHtml i))
                        (define frame (make-object frame% (string-append "OUTPUT for " (list-ref ListofFiles i)) #f 800 800))
                        (define text (make-object text%))
                        (define editor-canvas (make-object editor-canvas% frame text))]
                  (send text set-styles-sticky #f)
                  (send text auto-wrap #t)
                  (send frame show #t)
;                  (send text begin-edit-sequence #f)
                  (render-html text a-html)
;                  (send text end-edit-sequence)
                  ))))

; render : text% -> (void)
; Called to render all test cases for render-html on a-text.
(define (render a-text)
  (send a-text insert desc-text)
  (test-suite html-files))

(render (make-object text%))
;(define ahtml (call-with-input-file "./testcases/render-table/case6.html" html:read-html))
;(define htmlfullcontent (html:html-full-content ahtml))
;(define thebody (first (html:html-full-content ahtml)))
;(define bodyfullcontent (html:html-full-content thebody))
;(define thetable (first (rest (rest bodyfullcontent))))
;
;(define nsm (generate-naive-structural-matrix thetable))
;(define a-text (make-object text%))
;(define frame (make-object frame% "" #f 800 800))
;(define editor-canvas (make-object editor-canvas% frame a-text))
;(send frame reflow-container)
;
;(define table-contents (html:html-full-content thetable))
;(define table-attribs (html:html-element-attributes thetable))
;(define listofcols (filter (lambda (x) (or (html:col? x)
;                                           (html:colgroup? x)))
;                           table-contents))
;(define-values (text-width text-height) (send a-text get-max-view-size))
;(define cellspacing (string->exact-non-negative-integer
;                     (get-attribute-value table-attribs 'cellspacing "0")
;                     0))
;(define cellpadding (string->exact-non-negative-integer
;                     (get-attribute-value table-attribs 'cellpadding "0")
;                     0))
;(define border (string->exact-non-negative-integer
;                (get-attribute-value table-attribs 'border "0")
;                0))
;(define explicit-table-width (length->pixels
;                              (get-attribute-value table-attribs 'width "0")
;                              text-width
;                              0))
;; padded structural-matrix
;(define extended-structural-matrix
;  (extend-col-spans nsm listofcols))
;; maximum width, in pixels, of the display available to table within contraints imposed by table's author
;(define available-space (cond [(zero? explicit-table-width)
;                               (- text-width (* cellspacing (add1 (count-columns extended-structural-matrix))))]
;                              [else
;                               (- explicit-table-width (* cellspacing (add1 (count-columns extended-structural-matrix))))]))
;; resizes columns of percentage length
;(define matrix-with-resolved-%length 
;  (resize-columns extended-structural-matrix
;                  listofcols  
;                  (lambda (attribs default-width-value)
;                    (determine-percent-length attribs
;                                              available-space
;                                              default-width-value))
;                  resize-column-logic/percent))
;; evens up all row heights and column widths
;(make-rows-and-columns-even! matrix-with-resolved-%length)
;; removes "ghost" columns and makes row heights and column widths even
;(define nsm-without-ghost-columns (remove-ghost-columns matrix-with-resolved-%length))
;(define implicit-table-width (get-implicit-table-width matrix-with-resolved-%length cellspacing border))
;(define total-portions (cond [(empty? listofcols) 0]
;                             [else (apply + (map get-portions listofcols))]))
;(define total-columns (length (transpose matrix-with-resolved-%length)))
;; visible space remaining in the frame
;(define remaining-available-space (- explicit-table-width implicit-table-width))
;(define scale (cond [(zero? total-columns) 0]
;                    [(zero? total-portions) (inexact->exact (round (/ remaining-available-space total-columns)))]
;                    [else (inexact->exact (round (/ remaining-available-space total-portions)))]))
;; resizes columns of proportional width OR all columns in order to stretch table
;(if (and (> total-portions 0) (> explicit-table-width implicit-table-width))
;    (resize-columns 
;     nsm-without-ghost-columns
;     listofcols 
;     (lambda (attribs default)
;       (* scale (get-attributes-width-proportion attribs)))
;     resize-column-logic/proportion!)
;    (if (and (<= total-portions 0) (> explicit-table-width implicit-table-width))
;        (begin 
;          (scale-matrix-to-proportion! 
;           nsm-without-ghost-columns
;           remaining-available-space)
;          nsm-without-ghost-columns)))
;; resizes cells and snips for merged-cells
;(setup-merged-cells! nsm-without-ghost-columns cellspacing)