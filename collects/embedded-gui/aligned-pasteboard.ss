(module aligned-pasteboard mzscheme
  
  ;;;;;;;;;;;
  ;; Debug
  
  (define (wrap x) (write x) x)
  
  (define-syntax (override/trace stx)
    (syntax-case stx ()
      [(_ id ...)
       (with-syntax ([(super ...) (generate-temporaries #'(id ...))])
         #'(begin (rename [super id] ...)
                  (define/override (id . args)
                    (printf "~s: ~s~n" (quote id) args)
                    (apply 
                     (case-lambda
                       [() (super)]
                       [(a) (super a)]
                       [(a b) (super a b)]
                       [(a b c) (super a b c)]
                       [(a b c d) (super a b c d)]
                       [(a b c d e) (super a b c d e)]
                       [(a b c d e f) (super a b c d e f)]
                       [(a b c d e f g) (super a b c d e f g)]
                       [(a b c d e f g h) (super a b c d e f g h)]
                       [(a b c d e f g h i) (super a b c d e f g h i)])
                     args))
                  ...))]))
  
  (provide
   aligned-pasteboard%
   horizontal-alignment%
   vertical-alignment%
   grid-alignment%)
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "etc.ss")
   (lib "list.ss")
   (lib "match.ss")
   (prefix a: "alignment.ss")
   (lib "click-forwarding-editor.ss" "mrlib")
   (lib "snip-lib.ss" "mrlib" "private" "aligned-pasteboard"))
  
  (define aligned-pasteboard%
    (class (click-forwarding-editor-mixin pasteboard%)
      (inherit begin-edit-sequence end-edit-sequence)
      (field
       [alignment (new vertical-alignment%)]
       [lock-alignment? false]
       [needs-alignment? false])
      
      (define/public (add i) (send alignment add i))
      
      #|
        snip : snip% object
        before : snip% object or #f
        x : real number
        y : real number
      |#
      (rename [super-after-insert after-insert])
      (define/override (after-insert snip before x y)
        (super-after-insert snip before x y)
        (realign))
      
      #|
        snip : snip% object
      |#
      (rename [super-after-delete after-delete])
      (define/override (after-delete snip)
        (super-after-delete snip)
        (realign))
      
      #|
        snip : snip% object
        redraw-now? : boolean
      |#
      (rename [super-resized resized])
      (define/override (resized snip redraw-now?)
        (super-resized snip redraw-now?)
        (realign))
      
      (rename [super-on-display-size on-display-size])
      (define/override (on-display-size)
        (super-on-display-size))
      
      (define/public (lock-alignment lock?)
        (set! lock-alignment? lock?)
        (when (and needs-alignment? (not lock-alignment?))
          (realign))
        (if lock?
            (begin-edit-sequence)
            (end-edit-sequence)))
      
      ;; Is this sound? Seems like if I allow a needs-alignment set
      ;; when I'm inside thatr fluid-let I could get a loop. Probably
      ;; not experiencing problems now because I only align from a
      ;; call to (lock-alignment false)
      (define/public (realign)
        (if lock-alignment?
            (set! needs-alignment? true)
            (fluid-let ([lock-alignment? true])
              (send alignment set-min-sizes)
              (send alignment align))))
      
      (super-new)
      (send alignment set-pasteboard this)))
  
  (define alignment<%>
    (interface ()
      set-min-sizes
      align
      get-min-width
      get-min-height
      stretchable-width?
      stretchable-height?
      show))
  
  (define (vert/horiz-alignment type)
    (class* object% (alignment<%>)
      
      (init-field (parent false))
      
      (field
       [pasteboard false]
       [children empty]
       [min-width 0]
       [min-height 0]
       [show? true])
      
      ;; need base class for this method
      (define (show/hide-child child show?)
        (if (is-a? child alignment<%>)
            (send child show show?)
            (if show?
                (send pasteboard insert child)
                (send pasteboard release-snip child))))
      
      ;; STATUS: This function (through lock-alignment false) invokes a call
      ;; to realign of the pasteboard even when this alignement has show? = false
      ;; so the call is not needed.
      (define/public (add child)
        (set! children (append children (list child)))
        (send pasteboard lock-alignment true)
        (cond
          [(is-a? child snip%)
           (when show?
             (send pasteboard insert child false))]
          [(is-a? child alignment<%>)
           (send child set-pasteboard pasteboard)])
        (send pasteboard lock-alignment false))
      
      (define/public (get-min-width)
        (if show?
            min-width
            0))
      (define/public (get-min-height)
        (if show?
            min-height
            0))
      (define/public (set-pasteboard pb) (set! pasteboard pb))
      (define/public (stretchable-width?) false)
      (define/public (stretchable-height?) false)
      (define/public (show bool)
        (unless (boolean=? bool show?)
          (set! show? bool)
          (send pasteboard lock-alignment true)
          (for-each (lambda (c)
                      (show/hide-child c bool))
                    children)
          (send pasteboard lock-alignment false)))
      
      (define/public align
        (opt-lambda ((x-offset 0) (y-offset 0))
          
          (define move
            (match-lambda*
              [(child ($ a:rect ($ a:dim x _ _) ($ a:dim y _ _)))
               (move-child child pasteboard (+ x x-offset) (+ y y-offset))]))
          (when show?
            (for-each move
                      children
                      (a:align type 100 100 ;; FIXME
                               (map build-rect children))))))
      
      (define/public (set-min-sizes)
        (when show?
          (for-each
           (lambda (child)
             (when (is-a? child alignment<%>)
               (send child set-min-sizes)))
           children)
          (let-values ([(x-accum y-accum)
                        (if (symbol=? type 'vertical)
                            (values vacuous-max +)
                            (values + vacuous-max))])
            (set! min-width
                  (apply x-accum
                         (map child-width
                              children)))
            (set! min-height
                  (apply y-accum
                         (map child-height
                              children))))))
      
      (super-new)
      (when parent (send parent add this))))
  
  (define vertical-alignment% (vert/horiz-alignment 'vertical))
  (define horizontal-alignment% (vert/horiz-alignment 'horizontal))
  
  (define grid-alignment%
    (class* object% (alignment<%>)
      (init-field
       columns
       (parent false))
      (field
       [pasteboard false]
       [rows empty]
       [row-heights 0]
       [column-widths 0]
       [show? true])
      
      ;; need base class for this method
      (define (show/hide-child child show?)
        (if (is-a? child alignment<%>)
            (send child show show?)
            (if show?
                (send pasteboard insert child)
                (send pasteboard release-snip child))))
      
      (define/public (add row)
        (set! rows (append rows (list row)))
        (unless (= (vector-length row) columns)
          (error 'add "Invalid number of rows"))
        (send pasteboard lock-alignment true)
        (let loop ([column 0])
          (unless (>= column columns)
            (let ([child (vector-ref row column)])
              (cond
                [(is-a? child snip%)
                 (when show?
                   (send pasteboard insert child false))]
                [(is-a? child alignment<%>)
                 (send child set-pasteboard pasteboard)])
              (loop (add1 column)))))
        (send pasteboard lock-alignment false))
      
      (define/public (set-min-sizes)
        
        (set! column-widths
              (map
               (lambda (column)
                 (apply vacuous-max
                        (map (lambda (row)
                               (child-width
                                (vector-ref row column)))
                             rows)))
               (build-list columns identity)))
        
        (set! row-heights
              (map
               (lambda (row)
                 (apply vacuous-max
                        (map (lambda (column)
                               (child-height
                                (vector-ref row column)))
                             (build-list columns identity))))
               rows)))
      
      (define/public align
        (opt-lambda ((x-offset 0) (y-offset 0))
          (define (align-row row init-x y)
            (let xloop ([x init-x]
                        [column 0]
                        [widths column-widths])
              (unless (or (>= column columns) (empty? widths))
                (move-child (vector-ref row column) pasteboard x y)
                (xloop (+ x (first widths))
                       (add1 column)
                       (rest widths)))))
          (when show?
            (let yloop ([y y-offset]
                        [the-rows rows]
                        [heights row-heights])
              (unless (or (empty? the-rows) (empty? heights))
                (align-row (first the-rows) x-offset y)
                (yloop (+ y (first heights))
                       (rest the-rows)
                       (rest heights)))))))
      
      (define/public (get-min-width)
        (if show?
            (apply + column-widths)
            0))
      (define/public (get-min-height)
        (if show?
            (apply + row-heights)
            0))
      
      (define/public (show bool)
        (define (show/hide-row row)
          (let loop ([column 0])
            (unless (>= column columns)
              (let ([child (vector-ref row column)])
                (show/hide-child child bool)
                (loop (add1 column))))))
        (unless (boolean=? bool show?)
          (set! show? bool)
          (send pasteboard lock-alignment true)
          (for-each show/hide-row rows)
          (send pasteboard lock-alignment false)))
      
      (define/public (stretchable-width?) false)
      (define/public (stretchable-height?) false)
      (define/public (set-pasteboard pb) (set! pasteboard pb))
      
      (super-new)
      (when parent (send parent add this))))
  
  ;; build-rect ((is-a?/c snip%) . -> . rect?)
  ;; makes a new default rect out of a snip
  (define (build-rect item)
    (cond
      [(is-a? item snip%)
       (a:make-rect
        (a:make-dim 0 (snip-min-width item) (stretchable-width? item))
        (a:make-dim 0 (snip-min-height item) (stretchable-height? item)))]
      [(is-a? item alignment<%>)
       (a:make-rect
        (a:make-dim 0 (send item get-min-width) (send item stretchable-width?))
        (a:make-dim 0 (send item get-min-height) (send item stretchable-height?)))]))
  
  (define (child-height item)
    (cond
      [(is-a? item snip%) (snip-min-height item)]
      [(is-a? item alignment<%>) (send item get-min-height)]))
  
  (define (child-width item)
    (cond
      [(is-a? item snip%) (snip-min-width item)]
      [(is-a? item alignment<%>) (send item get-min-width)]))
  
  (define (move-child child pasteboard x y)
    (cond
      [(is-a? child snip%)
       (send pasteboard move-to child x y)]
      [(is-a? child alignment<%>)
       (send child align x y)]))
  
  (define (vacuous-max . n)
    (if (empty? n)
        0
        (apply max n)))
  
  #|
  (define f (new frame% (label "f") (height 500) (width 500)))
  (send f show true)
  (define a1 (new aligned-pasteboard% ))
  (define c (new editor-canvas% (editor a1) (parent f)))
  (define a2 (new horizontal-alignment% (parent a1)))
  (define a3 (new horizontal-alignment% (parent a1)))
  (define a4 (new grid-alignment% (parent a1) (columns 4)))
  (send a2 add (make-object string-snip% "One"))
  (send a2 add (make-object string-snip% "Two"))
  (send a3 add (make-object string-snip% "Three"))
  (send a3 add (make-object string-snip% "Four"))
  (send a4 add (vector (make-object string-snip% "This is really long")
                       (new editor-snip% (editor (new text%)))
                       (make-object string-snip% "short")
                       (make-object string-snip% "meduim")))
  (send a4 add (vector (make-object string-snip% "short")
                         (make-object string-snip% "This is really long")
                         (new editor-snip% (editor (new text%)))
                         (make-object string-snip% "meduim")))
  (send f show true)
  |#
  )
