(module test-text mzscheme
  
  (require
   (lib "class.ss")
   (lib "framework.ss" "framework")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "aligned-pasteboard.ss" "mrlib"))
  
  (provide
   test:text%
   test:editor-snip%)
  
  (define test:text%
    (class scheme:text%
      (inherit set-modified)
      
      ;; autosave? (-> boolean?)
      ;; called to test whether to auto save this editor
      (define/override (autosave?)
        false)
      
      ;; on-insert (number? number? . -> . void?)
      ;; called when something is inserted into the editor
      (rename [super-after-insert on-insert])
      (define/override (on-insert start len)
        (set-modified true)
        (super-after-insert start len))
      
      (super-instantiate ())))
  
  (define test:editor-snip%
    (class* editor-snip% (aligned-snip<%>)
      (inherit get-editor get-margin)
      
      (init
       (stretchable-width true)
       (stretchable-height true))
      
      (field
       (stretchable-width-field stretchable-width)
       (stretchable-height-field stretchable-height))
      
      (public (stretchable-width-method stretchable-width)
              (stretchable-height-method stretchable-height))
      
      ;; stretchable-width (case-> (Boolean . -> . (void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards width
      (define stretchable-width-method
        (case-lambda
          [(value) (set! stretchable-width-field value)]
          [() stretchable-width-field]))
      
      ;; stretchable-height (case-> (Boolean . -> .(void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards height
      (define stretchable-height-method
        (case-lambda
          [(value) (set! stretchable-height-field value)]
          [() stretchable-height-field]))
      
      ;; get-aligned-min-width (-> number?)
      ;; the minimum width of the snip based on the children
      (define/public (get-aligned-min-width)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (+ (unbox left) (unbox right))))
      
      ;; get-aligned-min-height (-> number?)
      ;; the minimum height of the snip based on the children
      (define/public (get-aligned-min-height)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (+ (unbox top) (unbox bottom)
             (* 15 (add1 (send (get-editor) last-line))))))
      
      (super-instantiate ())
      ))
  )
