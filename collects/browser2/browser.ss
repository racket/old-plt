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
    
    ;; A browser-frame% is a frame% that handles keyboard events differently.
    ;; Guarantees that when Enter key is pressed in the frame, it reaches the
    ;; receiver's normal key handling mechanism.
    (define browser-frame%
      (class frame% (label parent width height x y style)
        (rename [super-on-subwindow-char on-subwindow-char])
        (sequence (super-init label parent width height x y style))
        (override
          [on-subwindow-char
           (lambda (receiver event)
             (local [(define key (send event get-key-code))]
               (cond [(or (and (char? key)
                               (char=? key #\return))
                          (and (symbol? key)
                               (symbol=? key 'numpad-enter)))
                      (send receiver on-subwindow-char receiver event)]
                     [else (super-on-subwindow-char receiver event)])))])))
    
    ;; A browser-editor-canvas% is an editor-canvas% that adjusts its minimum
    ;; height automatically to the new window size.
    (define browser-editor-canvas%
      (class editor-canvas% (frame editor)
        (rename [super-on-size on-size])
        (sequence (super-init frame editor))
        (override
          [on-size
           (lambda (width height)
             (send this min-height height)
             (super-on-size width height))])))
    
    ;; A browser-text% is a text% that stores the html's pages base-path, local anchors, and methods for
    ;; setting and getting those variables.
    (define browser-text%
      (class text% ()
        (public
          [base-path #f]
          [local-anchors empty]
          [set-base-path!
           (lambda (a-URL)
             (set! base-path a-URL))]
          [get-base-path
           (lambda ()
             base-path)]
          [reset-base-path!
           (lambda ()
             (set! base-path #f))])
        (sequence (super-init))))
    
    ;; CONST DEFS
    (define WIDTH 800)
    (define HEIGHT 700)
    (define MAX_BACK_HIST 5)
    (define MAX_FORW_HIST 5)
    
    ;; VARIABLE DEFS
    (define the-history (make-vector 10 #f))
    (define history-index 0)
    
    ; slide-history-right! : browser-text% -> void
    ; Makes new history, throwing away oldest element
    ; and adding the newest.
    (define (slide-history-right! a-browser-text)
      (do ((vec (make-vector (vector-length the-history)))
           (i 1 (+ i 1)))
        ((= i (vector-length the-history)) vec)
        (vector-set! vec i i)))
      
       
    ; add-to-history : browser-text% -> void
    (define (add-to-history a-browser-text)
      (cond [(< history-index (sub1 (vector-length the-history)))
             (vector-set! the-history history-index a-browser-text)
             (set! history-index (add1 history-index))]
            [else (slide-history-right! a-browser-text)]))
             
    
    
    (define back-list empty)
    (define forward-list empty)
    (define current-editor (make-object browser-text%)) ; holds state of browser's display.
    (define frame (make-object browser-frame% "" #f WIDTH HEIGHT #f #f null))
    (define top-hpane (make-object horizontal-pane% frame))
    (define btm-hpane (make-object horizontal-pane% frame))
    
    (define open-url-textbox (make-object text-field% "Location:" btm-hpane 
                               (lambda (a-textfield a-control-event)
                                 (cond [(symbol=? 'text-field-enter (send a-control-event get-event-type))
                                        (browse-url (string->url (send a-textfield get-value)))]
                                       [else (void)]))
                               "" '(single)))
    
    (define back-btn (make-object button% "Back" top-hpane 
                       (lambda (a-btn a-control-event) 
                         (cond [(empty? back-list) (void)]
                               [else 
                                (set! forward-list (cons current-editor forward-list))
                                (set! current-editor (first back-list))
                                (set! back-list (rest back-list))
                                (send editor-canvas set-editor current-editor)
                                (if (send current-editor get-base-path)
                                    (begin
                                      (send frame set-label (url->string (send current-editor get-base-path)))
                                      (send open-url-textbox set-value (url->string (send current-editor get-base-path))))
                                    (begin
                                      (send frame set-label "")
                                      (send open-url-textbox set-value "")))]))
                       '(border)))
    (define forward-btn (make-object button% "Forward" top-hpane 
                          (lambda (a-btn a-control-event) 
                            (cond [(empty? forward-list) (void)]
                                  [else 
                                   (set! back-list (cons current-editor back-list))
                                   (set! current-editor (first forward-list))
                                   (set! forward-list (rest forward-list))
                                   (send editor-canvas set-editor current-editor)
                                   (if (send current-editor get-base-path)
                                       (begin
                                         (send frame set-label (url->string (send current-editor get-base-path)))
                                         (send open-url-textbox set-value (url->string (send current-editor get-base-path))))
                                       (begin
                                         (send frame set-label "")
                                         (send open-url-textbox set-value "")))]))
                          '(border)))
    (define reload-btn (make-object button% "Reload" top-hpane (lambda (x y)
                                                                 (local [(define base-path (send current-editor get-base-path))]
                                                                   (if base-path
                                                                       (begin
                                                                         (send current-editor erase)
                                                                         (render-html-page current-editor base-path
                                                                                           (call/input-url base-path get-pure-port html:read-html)))
                                                                       (void))))))

    (define home-btn (make-object button% "Home" top-hpane (lambda (x y) (void)) '(border)))
    (define print-btn (make-object button% "Print" top-hpane (lambda (x y) (void)) '(border)))
    (define history-choice-listbox (make-object choice% "History:" (list "http://www.cs.rice.edu" "http://www.rice.edu/") top-hpane 
                                     (lambda (a-choice a-control-event) 
                                       (local [(define index (send a-choice get-selection))
                                               (define selection (send a-choice get-string index))]
                                         (browse-url (string->url selection))))))

    (define editor-canvas (make-object browser-editor-canvas% frame current-editor))
        
    
    ;; set properties of containers
    ; stretches editor-canvas's height to that of frame, forcing top-hpane and btm-hpane to use minimum height necessary
    (send editor-canvas min-height HEIGHT)
    
    ; browse-url : url -> void
    ; Creates a new editor for the url and changes the state of currently displayed editor to be new editor.
    (define (browse-url a-url)
      (local [(define scheme (url-scheme a-url))
              (define fragment (url-fragment a-url))
              (define path (url-path a-url))
              (define file-suffix (get-file-suffix path))
              (define text (make-object browser-text%))
              (define (return-all-but-last a-list)
                (cond [(empty? a-list) empty]
                      [(empty? (rest a-list)) empty]
                      [else (cons (first a-list) (return-all-but-last (rest a-list)))]))]
        (if (< (length back-list) MAX_BACK_HIST)
            (set! back-list (cons current-editor back-list))
            (set! back-list (cons current-editor (return-all-but-last back-list))))
        (set! current-editor text)
        (send text set-base-path! a-url)
        (send editor-canvas set-editor current-editor)
        (send frame show #t)
        (send frame set-label (url->string a-url))
        (send open-url-textbox set-value (url->string a-url))
        (send current-editor set-styles-sticky #f)
        (send current-editor auto-wrap #t)
        (send current-editor begin-edit-sequence)
        (render-html-page current-editor a-url (call/input-url a-url get-pure-port html:read-html))
        (send current-editor end-edit-sequence)))
    
    ; get-current-editor : -> browser-text%
    ; Returns the currently displayed editor.
    (define (get-current-editor)
      current-editor)
    
    ; get-base-path : -> url
    ; Returns the base-path of the currently displayed editor.
    (define (get-base-path)
      (send current-editor get-base-path))
    
    ; set-frame-label : string -> void
    ; Sets the frame's label.
    (define (set-frame-label a-string)
      (send frame set-label a-string)))
  #f
  mred^
  mzlib:file^
  mzlib:function^
  xml^
  (html : html^)
  mzlib:url^)

(define testanchors-url (string->url "file:///home/bonfield/plt/collects/browser2/testcases/testanchors.html" ))
(define testparser-url (string->url "file:///home/bonfield/plt/collects/browser2/testcases/testparser.html"))
(define robby-url (string->url "http://www.cs.rice.edu/~robby/main-2000.shtml"))
(define plt-doc-url (string->url "file:///home/bonfield/plt/collects/doc/drscheme/index.html"))
(define utah-plt-url (string->url "file:///home/bonfield/plt/collects/browser2/testcases/utah-plt-doc.html"))
(define ol-url (string->url "file:///home/bonfield/plt/collects/browser2/test.html"))
(define basics-url (string->url "file:///home/bonfield/plt/collects/browser2/testcases/render-html/basic-renderers.html"))
(define a-html (call/input-url testparser-url get-pure-port html:read-html))
(define html-contents (html:html-full-content a-html))  ; head and body
(define head-contents (map html:html-full-content
                           (filter (lambda (x)
                                     (html:head? x)) html-contents)))
(define body-contents (map html:html-full-content
                           (filter (lambda (x)
                                     (html:body? x)) html-contents)))
