(require-library "refer.ss")
(require-library "macro.ss")
(require-library "function.ss")

;; A browser-frame% is a frame% that handles keyboard events differently.
;; If the Enter key is pressed, it guarantees that the key event reaches
;; the receiver child.
(define browser-frame%
  (class frame% (label parent width height x y style)
    (rename [super-on-subwindow-char on-subwindow-char])
    (sequence (super-init label parent width height x y style))
    (override
      [on-subwindow-char
       (lambda (receiver event)
         (send receiver on-subwindow-char receiver event))])))


;; CONST DEFS
(define WIDTH 800)
(define HEIGHT 700)
(define page1 (make-object text%))
(define page2 (make-object text%))
(define page3 (make-object text%))
(define page4 (make-object text%))
(define page5 (make-object text%))
(define page6 (make-object text%))
(define page7 (make-object text%))
(send page1 insert "page 1!")
(send page2 insert "page 2!")
(send page3 insert "page 3!")
(send page4 insert "page 4!")
(send page5 insert "page 5!")
(send page6 insert "page 6!")
(send page7 insert "page 7!")

;; VARIABLE DEFS
(define back-list (list page7 page6 page5 page4 page3 page2 page1))
(define forward-list empty)
(define current-editor (make-object text%)) ; holds state of browser's display.
(define frame (make-object browser-frame% "" #f WIDTH HEIGHT #f #f null))
(define top-hpane (make-object horizontal-pane% frame))
(define btm-hpane (make-object horizontal-pane% frame))

(define back-btn (make-object button% "Back" top-hpane 
                   (lambda (a-btn a-control-event) 
                     (cond [(empty? back-list) (void)]
                           [else 
                            (printf "focus: ~s~n" (send frame get-focus-object))
                            (set! forward-list (cons current-editor forward-list))
                            (set! current-editor (first back-list))
                            (set! back-list (rest back-list))
                            (send editor-canvas set-editor current-editor)]))
                   '(border)))
(define forward-btn (make-object button% "Forward" top-hpane 
                      (lambda (a-btn a-control-event) 
                        (cond [(empty? forward-list) (void)]
                              [else 
                               (set! back-list (cons current-editor back-list))
                               (set! current-editor (first forward-list))
                               (set! forward-list (rest forward-list))
                               (send editor-canvas set-editor current-editor)]))
                      '(border)))
(define reload-btn (make-object button% "Reload" top-hpane (lambda (x y) (void)) '(border)))
(define home-btn (make-object button% "Home" top-hpane (lambda (x y) (void)) '(border)))
(define print-btn (make-object button% "Print" top-hpane (lambda (x y) (void)) '(border)))
(define history-choice-listbox (make-object choice% "History:" (list "http://www.rice.edu/~bonfield") top-hpane (lambda (x y) (void))))

(define open-url-textbox (make-object text-field% "Location:" btm-hpane 
                           (lambda (a-textfield a-control-event)
                             (cond [(symbol=? 'text-field-enter (send a-control-event get-event-type))
                                    (printf "value: ~s~n" (send a-textfield get-value))]
                                   [else (void)]))
                           "" '(single)))

(define editor-canvas (make-object editor-canvas% frame current-editor))

;; set properties of containers
; stretches editor-canvas's height to that of frame, forcing top-hpane and btm-hpane to use minimum height necessary
(send editor-canvas min-height HEIGHT)

(send frame show #t)

(send current-editor insert "CURRENT EDITOR")