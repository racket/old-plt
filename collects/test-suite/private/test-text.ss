(module test-text mzscheme
  
  (require
   (lib "class.ss")
   (lib "framework.ss" "framework")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "aligned-pasteboard.ss" "mrlib")
   "interfaces.ss")
  
  (provide
   def-text%
   call-text%
   expected-text%
   actual-text%
   test-text%
   base-snip%
   actual-snip%)
  
  (define *disable-color* "AliceBlue")
  
  (define (grey-editor-snip-mixin super%)
    (class super%
      (rename [super-draw draw])
      (inherit get-admin)
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [admin (get-admin)]
              [wb (box 0)]
              [hb (box 0)])
          (when admin
            (send admin get-view #f #f wb hb this)
            (send dc set-pen (send the-pen-list find-or-create-pen *disable-color* 1 'solid))
            (send dc set-brush (send the-brush-list find-or-create-brush *disable-color* 'solid))
            
            ;; should be use inset instead of 1 and 2
            (send dc draw-rectangle (+ x 1) (+ y 1)
                  (max 0 (- (unbox wb) 2))
                  (max 0 (- (unbox hb) 2)))
            
            (send dc set-pen old-pen)
            (send dc set-brush old-brush)))
        (super-draw dc x y left top right bottom dx dy draw-caret))
      (super-instantiate ())))
  
  (define (grey-editor-mixin super%)
    (class super%
      (rename [super-on-paint on-paint])
      (inherit get-admin)
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (when before?
          (let ([old-pen (send dc get-pen)]
                [old-brush (send dc get-brush)]
                [admin (get-admin)]
                [wb (box 0)]
                [hb (box 0)])
            (send dc set-pen (send the-pen-list find-or-create-pen *disable-color* 1 'solid))
            (send dc set-brush (send the-brush-list find-or-create-brush *disable-color* 'solid))
            (send admin get-max-view #f #f wb hb #t)
            (send dc draw-rectangle (+ dx 0) (+ dy 0) (unbox wb) (unbox hb))
            (send dc set-pen old-pen)
            (send dc set-brush old-brush)))
        (super-on-paint before? dc left top right bottom dx dy draw-caret))
      (super-instantiate ())))
  
  (define base-text%
    (class scheme:text%
      (inherit set-modified refresh-delayed?)
      (init-field case)
      
      ;; autosave? (-> boolean?)
      ;; called to test whether to auto save this editor
      (define/override (autosave?) false)
      
      ;; after-insert (number? number? . -> . void?)
      ;; called when something is inserted into the editor
      (rename [super-after-insert after-insert])
      (define/override (after-insert start len)
        (clear-highlighting)
        (super-after-insert start len))
      
      ;; after-delete (number? number? . -> . void?)
      ;; called when something is deleted from the editor
      (rename [super-after-delete after-delete])
      (define/override (after-delete start len)
        (clear-highlighting)
        (super-after-delete start len))
      
      ;; clear-highlighting
      ;; globally clear highlighting
      (define/private (clear-highlighting)
          (let ([editor 
                 (with-handlers ([exn? (lambda (exn) false)])
                   (send (send case get-admin) get-editor))])
            (when editor (send editor clear-highlighting))))
      
      ;; get-keymaps (-> (listof keymap%))
      ;; the list of keymaps associated with this text
      (rename [super-get-keymaps get-keymaps])
      (define/override (get-keymaps)
        (let ([keymap (make-object keymap%)])
          (send keymap add-function "tab-ahead"
                (lambda (ignored event)
                  (send case tab-ahead this)))
          (send keymap add-function "tab-back"
                (lambda (ignored event)
                  (send case tab-back this)))
          (send keymap map-function "tab" "tab-ahead")
          (send keymap map-function "s:tab" "tab-back")
          (cons keymap (super-get-keymaps))))
      
      (super-instantiate ())
      ))
  
  (define base-snip%
    (class (aligned-snip-mixin editor-snip%)
      (super-instantiate ()
        (stretchable-width true)
        (stretchable-height false))))
  
  (define def-text%
    (class base-text%
      (super-instantiate ())))
  
  (define call-text%
    (class base-text%
      (super-instantiate ())))
  
  (define expected-text%
    (class base-text%
      (super-instantiate ())))
  
  (define actual-text%
    (class (grey-editor-mixin
            (text:hide-caret/selection-mixin base-text%))
      (inherit hide-caret lock)
      (super-instantiate ())
      (hide-caret true)
      (lock true)))
  
  (define test-text%
    (class base-text%
      (inherit insert)
      (super-instantiate ())
      (insert "equal?")))
  
  (define actual-snip%
    (grey-editor-snip-mixin base-snip%))
  )