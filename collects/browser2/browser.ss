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
    
    ; A service is one of:
    ; - 'get-history-size
    ; - 'reset-history
    ; - 'get-current-editor
    ; - 'add-editor!
    ; - 'print-history
    ; - 'go-back
    ; - 'go-forward
    ; - 'retro-history?
    ; - 'forward-history?
    ; - 'move-current-editor-to-index
    
    ; shift-vector-left : vector object -> vector
    ; Shifts contents of vector left one position and fills remainder with object.
    (define (shift-vector-left a-vector an-object)
      (local [(define length (vector-length a-vector))]
        (build-vector length
                      (lambda (index)
                        (if (< (add1 index) length)
                            (vector-ref a-vector (add1 index))
                            an-object)))))
    
    ; shift-choices-left -> void
    ; Shifts contents of choice% to the left one index.
    (define (shift-choices-left)
      (local [(define listof-choices empty)]
        (do ((hi-index (sub1 (send history-choice-control get-number)) (sub1 hi-index)))
          ((<= hi-index 1)
           (begin
             (send history-choice-control clear)
             (for-each (lambda (a-selection) (send history-choice-control append a-selection)) listof-choices)
             (send history-choice-control set-selection (sub1 (send history-choice-control get-number)))))
          (set! listof-choices (cons (send history-choice-control get-string hi-index) listof-choices)))))
      
    ; browser-history-interface : symbol -> #f or browser-text% or void
    ; Interface for the browser's history initialized at run-time.  Accepts commands
    ; and returns void or a browser-text%.
    (define browser-history-interface
      (local [(define HISTORY_SIZE 5) ; must be greater than 0
              (define ptr 0)
              (define current-editor-ptr 0)
              (define history-vector (make-vector HISTORY_SIZE #f))
              (define (error.msg symbol) (error "Unknown command, given ~s" symbol))
              
              ; get-history-size : -> integer
              (define (get-history-size) HISTORY_SIZE)
              
              ; get-current-editor-ptr : -> integer
              (define (get-current-editor-ptr) current-editor-ptr)

              ; get-ptr : -> integer
              (define (get-ptr) ptr)
              
              ; reset.history! -> void
              (define (reset.history!)
                (set! history-vector (make-vector HISTORY_SIZE #f))
                (set! ptr 0)
                (send history-choice-control clear))
              
              ; get-current-editor -> #f or browser-text%
              (define (get-current-editor)
                (vector-ref history-vector current-editor-ptr))
              
              ; add-editor! : browser-text% -> void
              (define (add-editor! a-browser-text)
                (if (<= ptr (sub1 HISTORY_SIZE))
                    (begin 
                      (vector-set! history-vector ptr a-browser-text)
                      (if (zero? ptr) (void) (set! current-editor-ptr ptr))
                      (set! ptr (add1 ptr))
                      (if (send a-browser-text get-base-path)
                          (begin
                            (send history-choice-control append (url->string (send a-browser-text get-base-path)))
                            (send history-choice-control set-selection (sub1 (send history-choice-control get-number)))))
                      (send editor-canvas set-editor a-browser-text))
                    (begin
                      (set! history-vector (shift-vector-left history-vector a-browser-text))
                      (set! ptr HISTORY_SIZE)
                      (set! current-editor-ptr (sub1 ptr))
                      (shift-choices-left)
                      (if (send a-browser-text get-base-path)
                          (begin
                            (send history-choice-control append (url->string (send a-browser-text get-base-path)))
                            (send history-choice-control set-selection (sub1 (send history-choice-control get-number)))))
                      (send editor-canvas set-editor a-browser-text))))
              
              ; print-history : -> void
              (define (print-history)
                (for-each (lambda (object)
                            (printf "~v " object))
                          (vector->list history-vector))
                (printf "~n"))
              
              ; go-back : -> void or browser-text%
              (define (go-back)
                (if (and (> current-editor-ptr 0) (<= current-editor-ptr HISTORY_SIZE))
                    (if (is-a? (vector-ref history-vector (sub1 current-editor-ptr)) browser-text%)
                        (begin
                          (set! current-editor-ptr (sub1 current-editor-ptr))
                          (vector-ref history-vector current-editor-ptr))
                        (void))
                    (void)))
              
              ; go-forward : -> void or browser-text%
              (define (go-forward)
                (if (and (>= current-editor-ptr 0) (< current-editor-ptr (sub1 HISTORY_SIZE)))
                    (if (is-a? (vector-ref history-vector (add1 current-editor-ptr)) browser-text%)
                        (begin
                          (set! current-editor-ptr (add1 current-editor-ptr))
                          (vector-ref history-vector current-editor-ptr))
                        (void))
                    (void)))
              
              ; retro-history? : -> boolean
              (define (retro-history?)
                (if (zero? current-editor-ptr)
                    #f
                    #t))
              
              ; forward-history? : -> boolean
              (define (forward-history?)
                (if (and (< current-editor-ptr (sub1 ptr)) (< current-editor-ptr (sub1 HISTORY_SIZE)))
                    #t
                    #f))
              
              ; move-current-editor-to-index : integer -> browser-text% or #f
              ;(define (move-current-editor-to-index index)
              ;  (cond [(= index current-editor-ptr) (vector-ref history-vector current-editor-ptr)]
              ;        [
              ]
        
        (lambda (service)
          (cond [(symbol=? service 'get-history-size) get-history-size]
                [(symbol=? service 'get-current-editor-ptr) get-current-editor-ptr]
                [(symbol=? service 'get-ptr) get-ptr]
                [(symbol=? service 'reset-history) reset.history!]
                [(symbol=? service 'get-current-editor) get-current-editor]
                [(symbol=? service 'add-editor!) add-editor!]
                [(symbol=? service 'print-history) print-history]
                [(symbol=? service 'go-back) go-back]
                [(symbol=? service 'go-forward) go-forward]
                [(symbol=? service 'retro-history?) retro-history?]
                [(symbol=? service 'forward-history?) forward-history?]
                [(symbol=? service 'move-current-editor-to-index) move-current-editor-to-index]
                [else (error.msg service)]))))
    
    ; A history-interface-command is one of:
    
    ; get-history-size : -> integer
    (define get-history-size (browser-history-interface 'get-history-size))
    
    ; get-current-editor-ptr -> integer
    (define get-current-editor-ptr (browser-history-interface 'get-current-editor-ptr))
    
    ; get-ptr -> integer
    (define get-ptr (browser-history-interface 'get-ptr))
    
    ; reset-browser-history! -> void
    (define reset-browser-history! (browser-history-interface 'reset-history))
    
    ; get-current-editor : -> browser-text% or #f
    (define get-current-editor (browser-history-interface 'get-current-editor))
    
    ; add-editor! : -> void
    (define add-editor! (browser-history-interface 'add-editor!))
    
    ; print-history : -> void or browser-text% or #f
    (define print-history (browser-history-interface 'print-history))
    
    ; go-back -> void or browser-text%
    (define go-back (browser-history-interface 'go-back))
        
    ; go-forward -> void or browser-text%
    (define go-forward (browser-history-interface 'go-forward))
    
    ; retro-history? : -> boolean
    (define retro-history? (browser-history-interface 'retro-history?))

    ; forward-history? : -> boolean
    (define forward-history? (browser-history-interface 'forward-history?))
    
    ; move-current-editor-to-index : integer -> browser-text% or #f
    (define move-current-editor-to-index (browser-history-interface 'move-current-editor-to-index))

    ;; CONST DEFS
    (define WIDTH 800)
    (define HEIGHT 700)
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
                         (local [(define prev (go-back))]
                           (cond [(void? prev) 
                                  (send back-btn enable #f)
                                  (void)]
                                 [else
                                  (if (retro-history?) (send back-btn enable #t) (send back-btn enable #f))
                                  (if (forward-history?) (send forward-btn enable #t) (send forward-btn enable #f))
                                  (send editor-canvas set-editor prev)
                                  (if (send prev get-base-path)
                                      (begin
                                        (send frame set-label (url->string (send prev get-base-path)))
                                        (send open-url-textbox set-value (url->string (send prev get-base-path))))
                                      (begin
                                        (send frame set-label "")
                                        (send open-url-textbox set-value "")))]))
                         '(border))))
    (send back-btn enable #f)
    
    (define forward-btn (make-object button% "Forward" top-hpane 
                          (lambda (a-btn a-control-event) 
                            (local [(define next (go-forward))]
                              (cond [(void? next) 
                                     (void)
                                     (send forward-btn enable #f)]
                                    [else
                                     (if (forward-history?) (send forward-btn enable #t) (send forward-btn enable #f))
                                     (if (retro-history?) (send back-btn enable #t) (send back-btn enable #f))
                                     (send editor-canvas set-editor next)
                                     (if (send next get-base-path)
                                         (begin
                                           (send frame set-label (url->string (send next get-base-path)))
                                           (send open-url-textbox set-value (url->string (send next get-base-path))))
                                         (begin
                                           (send frame set-label "")
                                           (send open-url-textbox set-value "")))]))
                            '(border))))
    (send forward-btn enable #f)
    
    (define reload-btn (make-object button% "Reload" top-hpane (lambda (x y)
                                                                 (local [(define current-editor (get-current-editor))
                                                                         (define base-path (send current-editor get-base-path))]
                                                                   (if base-path
                                                                       (begin
                                                                         (send current-editor erase)
                                                                         (render-html-page current-editor base-path
                                                                                           (call/input-url base-path get-pure-port html:read-html)))
                                                                       (void))))))

    (define home-btn (make-object button% "Home" top-hpane (lambda (x y) (void)) '(border)))
    (define print-btn (make-object button% "Print" top-hpane (lambda (x y) (void)) '(border)))
    (define history-choice-control (make-object choice% "History:" (list "http://") top-hpane 
                                     (lambda (a-choice a-control-event) 
                                       (local [(define index (send a-choice get-selection))
                                               (define selection (send a-choice get-string index))]
                                         (void)
                                         ;(browse-url (return-page-at-index index))))
                                     empty))))

    (define editor-canvas (make-object browser-editor-canvas% frame (get-current-editor)))
        
    
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
              (define text (make-object browser-text%))]
        (send text set-base-path! a-url)
        (add-editor! text) ; adds this editor to the browser-history
        (if (retro-history?)
            (send back-btn enable #t)
            (send back-btn enable #f))
        (if (forward-history?)
            (send forward-btn enable #t)
            (send forward-btn enable #f))
        (send frame show #t)
        (send frame set-label (url->string a-url))
        (send open-url-textbox set-value (url->string a-url))
        (send text set-styles-sticky #f)
        (send text auto-wrap #t)
        (send text begin-edit-sequence)
        (render-html-page text a-url (call/input-url a-url get-pure-port html:read-html))
        (send text end-edit-sequence)))

    
    ; get-base-path : -> url
    ; Returns the base-path of the currently displayed editor.
    (define (get-base-path)
      (send (get-current-editor) get-base-path))
    
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
