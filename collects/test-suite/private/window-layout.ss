(module window-layout mzscheme
  
  (require
   (lib "unit.ss")
   (lib "unitsig.ss")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "tool.ss" "drscheme")
   (lib "aligned-pasteboard.ss" "mrlib")
   (lib "match.ss")
   "interfaces.ss"
   "signatures.ss"
   "../extension.ss")
  
  (provide window-layout@)
  
  (define *save-icon* (build-path (collection-path "icons") "save.bmp"))
  (define *execute-icon* (build-path (collection-path "icons") "execute.bmp"))
  (define *break-icon* (build-path (collection-path "icons") "break.bmp"))
  (define *empty-icon* (build-path (collection-path "test-suite") "private" "icons" "empty.jpeg"))
  
  (define window-layout@
    (unit/sig window^
      (import drscheme:tool^ (super : window^))
      (define window%
        (class* super:window% (test-suite:window<%>)
          (inherit get-area-container)
          (inherit-field model new new-helper delete execute break save)
          
          ;; update-executing (boolean? . -> . void?)
          ;; called when the model changes execution modes
          (rename [super-update-executing update-executing])
          (define/override (update-executing executing?)
            (if executing?
                (begin
                  (send new-button enable false)
		  (send new-helper-button enable false)
                  (send delete-button enable false)
                  (send execute-button enable false)
                  (hide-error-panel))
                (begin
                  (send new-button enable true)
                  (send new-helper-button enable true)
                  (send delete-button enable true)
                  (send execute-button enable true)))
            (super-update-executing executing?))
          
          ;; update-modified (boolean? . -> . void?)
          ;; called when the model is modified or saved
          (rename [super-update-modified update-modified])
          (define/override (update-modified modified?)
            (if modified?
                (send save-button show true)
                (send save-button show false))
            (super-update-modified modified?))
          
          (super-instantiate ()
            (height 500))
          
          ;;;;;;;;;;;;;;;;
          ;; button bar ;;
          ;;;;;;;;;;;;;;;;
          
          (field
           [button-panel
            (instantiate horizontal-panel% ()
              (parent (get-area-container))
              (stretchable-height false))]
           [save-button
            (instantiate button% ()
              (label
               ((drscheme:unit:make-bitmap
                 "Save" *save-icon*)
                (get-area-container)))
              (parent button-panel)
              (callback (lambda (b e) (save))))]
           [spacer1
            (instantiate horizontal-pane% ()
              (stretchable-width true)
              (parent button-panel))]
	   [extra-buttons
	    (let ([p (instantiate horizontal-panel% ()
				  (stretchable-width true)
				  (parent button-panel))])
	      (for-each (lambda (v)
			  (let-values ([(name icon hook) (apply values v)])
			    (instantiate button% ()
					 (label
					  ((drscheme:unit:make-bitmap
					    name icon)
					   (get-area-container)))
					 (parent button-panel)
					 (callback (lambda (b e)
						     (hook this model))))))
			(test-suite-extensions)))]
           [spacer2
            (instantiate horizontal-pane% ()
              (stretchable-width true)
              (parent button-panel))]
           [new-button
            (instantiate button% ()
              (label
               ((drscheme:unit:make-bitmap
                 "New Test" *empty-icon*)
                (get-area-container)))
              (parent button-panel)
              (callback (lambda (b e) (new))))]
	   [new-helper-button
            (instantiate button% ()
              (label
               ((drscheme:unit:make-bitmap "New Helpers" *empty-icon*)
                (get-area-container)))
              (parent button-panel)
              (callback (lambda (b e) (new-helper))))]
           [delete-button
            (instantiate button% ()
              (label
               ((drscheme:unit:make-bitmap
                 "Delete" *empty-icon*)
                (get-area-container)))
              (parent button-panel)
              (callback (lambda (b e) (delete))))]
           [execute-button
            (instantiate button% ()
              (label
               ((drscheme:unit:make-bitmap
                 "Execute" *execute-icon*)
                (get-area-container)))
              (parent button-panel)
              (callback (lambda (b e) (execute))))]
           [break-button
            (instantiate button% ()
              (label
               ((drscheme:unit:make-bitmap
                 "Break" *break-icon*)
                (get-area-container)))
              (parent button-panel)
              (callback (lambda (b e) (break))))])
          
          (send save-button show false)
          
          ;;;;;;;;;;;;;;;;;;;
          ;; program-panel ;;
          ;;;;;;;;;;;;;;;;;;;
          
          (let* ([program-panel
                  (instantiate horizontal-panel% ()
                    (parent (get-area-container))
                    (style '(border))
                    (stretchable-height false))]
                 [program-label
                  (instantiate message% ()
                    (parent program-panel)
                    (label "Program to Test"))]
                 [program-canvas
                  (instantiate editor-canvas% ()
                    (parent program-panel)
                    (editor (send model get-program))
                    (style '(no-hscroll no-vscroll))
                    (stretchable-width true))]
                 [program-button
                  (instantiate button% ()
                    (label
                     ((drscheme:unit:make-bitmap
                       "Browse..." *empty-icon*)
                      (get-area-container)))
                    (parent program-panel)
                    (callback 
                     (lambda (button event)
                       (let ([filename (get-file)])
                         (when filename
                           (send model set-program filename))))))]
                 [open-button
                  (instantiate button% ()
                    (label
                     ((drscheme:unit:make-bitmap
                       "Open..." *empty-icon*)
                      (get-area-container)))
                     (parent program-panel)
                     (callback
                      (lambda (button event)
                        (let ([filename (send (send model get-program) get-text)])
                          (unless (string=? filename "")
                            (if (file-exists? filename)
                                (send (handler:edit-file filename) focus)
                                (message-box "Test-suite Error"
                                             (format "No such file ~a" filename))))))))])
            
            (send program-canvas set-line-count 1))
          
          ;; set-program (string? . -> . void?)
          ;; set the program to be evaluated
          (define/public (set-program p)
            (send model set-program p)
            (update-modified false))
          
          ;;;;;;;;;;;;;;;;;
          ;; error-panel ;;
          ;;;;;;;;;;;;;;;;;
          
          (field
           [error-place-holder
            (instantiate vertical-panel% ()
              (parent (get-area-container))
              (stretchable-height false)
              (stretchable-width true))]
           [error-panel
            (instantiate horizontal-panel% ()
              (parent error-place-holder)
              (style '(border))
              (stretchable-height false))]
           [label-panel
            (instantiate vertical-panel% ()
              (parent error-panel)
              (stretchable-width false)
              (stretchable-height false))]
           [error-text
            (instantiate (text:hide-caret/selection-mixin text:basic%) ()
              (auto-wrap true))]
           [ec
            (instantiate editor-canvas% ()
              (parent error-panel)
              (editor error-text)
              (style '(no-hscroll))
              (stretchable-height true)
              (stretchable-width true))])
          
          (send error-text hide-caret true)
          
          (instantiate message% ()
            (parent label-panel)
            (label "Test suite")
            (stretchable-width true))
          
          (instantiate message% ()
            (parent label-panel)
            (label "Error Message")
            (stretchable-width true))
          
          (instantiate button% ()
            (parent error-panel)
            (label "Hide")
            (callback
             (lambda (button event)
               (hide-error-panel)))
            (stretchable-height true))
          
          (send error-text lock true)
          (send ec set-line-count 3)
          (hide-error-panel)
          
          ;; get-error-handler (-> (string? exn? . -> . void?))
          ;; an error handler for displaying errors to the window
          (rename [super-get-error-handler get-error-handler])
          (define/override (get-error-handler)
            (letrec ([highlight-errors
                     (match-lambda
                       [() (send model set-has-highlighting true)]
                       [((text start stop) errors ...)
                        (let ([sd (make-object style-delta%)])
                          (send sd set-delta-background "lightpink")
                          (send text change-style sd start stop))
                        (highlight-errors errors)])])
              (lambda (msg exn)
                ((super-get-error-handler) msg exn)
                (send* error-text
                  (lock false)
                  (erase)
                  (lock true))
                ((drscheme:debug:make-debug-error-display-handler/text
                 (lambda () error-text)
                 (lambda (text thunk) (thunk))
                 (lambda (text info error-arrows) (highlight-errors info))
                 (lambda (msg exn)
                   (drscheme:rep:insert-error-in-text/highlight-errors
                    error-text
                    highlight-errors
                    msg exn
                    false)
                   (show-error-panel)))
                 msg exn))))
            
          ;; hide-error-panel (-> void?)
          ;; hide the error display
          (define/private (hide-error-panel)
            (when (member error-panel (send error-place-holder get-children))
              (send error-place-holder change-children
                    (lambda (l) empty))))
          
          ;; show-error-panel (-> void?)
          ;; show the error display
          (define/private (show-error-panel)
            (unless (member error-panel (send error-place-holder get-children))
              (send error-place-holder change-children
                    (lambda (l) (list error-panel)))))
          
          (instantiate aligned-editor-canvas% ()
            (parent (get-area-container))
            (editor model))
          ))
      ))
  )
