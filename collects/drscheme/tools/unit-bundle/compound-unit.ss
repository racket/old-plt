#|

A compound unit is a class with this data:
  bundle-manager                          ; imports
  (list-of (make-link bundle-manager
                      bundle-manager
                      sexp))              ; links
  bundle-manager                          ; exports

It supports these operations:

  get-imports : (-> bundle-manager<%>)
  set-imports : (bundle-manager<%> -> void)
  get-exports : (-> bundle-manager<%>)
  set-exports : (bundle-manager<%> -> void)
  get-links : (-> (list-of (make-link bundle-manager bundle-manager sexp)))
  set-links : ((list-of (make-link bundle-manager bundle-manager sexp)) -> void)
  
  create-view :  (-> frame)
     ;; puts the compound unit into the pasteboard
  
|#

(unit/sig drscheme:bundle:compound-unit^
  (import mred^
          framework^)
  
  (define compound-unit<%>
    (interface ()
      get-imports set-imports
      get-exports set-exports
      get-links set-links
      
      create-view))
  
  (define-struct link (import export exp))
  
  (define compound-unit%
    (class* object% (compound-unit<%>) (imports links exports)
      (public
        [get-imports (lambda () imports)]
        [set-imports (lambda (i) (set! imports i))]
        [get-exports (lambda () exports)]
        [set-exports (lambda (e) (set! exports e))]
        [get-links (lambda () links)]
        [set-links (lambda (l) (set! links l))])
      
      
      (public
        [create-view
         (lambda ()
           (let* ([frame (make-object frame:basic% "Compound Unit")]
                  [main-panel (make-object horizontal-panel% frame)]
                  [order-pb (make-object pasteboard%)]
                  [order-canvas (make-object editor-canvas% main-panel order-pb)]
                  [linking/signature-panel (make-object vertical-panel% main-panel)]
                  [linking-pb (make-object linking-pasteboard% this)]
                  [linking-canvas (make-object editor-canvas% linking/signature-panel linking-pb)]
                  [signature-panel (make-object horizontal-panel% linking/signature-panel '(border))]
                  [signature-control-panel (make-object vertical-panel% signature-panel)]
                  [signature-list-box (make-object list-box% #f null linking/signature-panel)]
                  [signature-message (make-object message% "Signature Files" signature-control-panel)]
                  [signature-button (make-object button% "Add File" signature-control-panel
                                                 (lambda xxx
                                                   (void)))])
             (send frame show #t)))])
      
      (sequence (super-init))))
  
  (define link-snip%
    (class editor-snip% (link)
      (sequence
        (let ([text (make-object text%)])
          (send (link-import link) create-view
                (lambda (snip)
                  (send text insert snip)))
          (send text insert #\newline)
          (send (link-export link) create-view
                (lambda (snip)
                  (send text insert snip)))
          (super-init text #t)))))
           
  
  (define linking-pasteboard%
    (class pasteboard% (compound-unit)
      (sequence
        (super-init)))))