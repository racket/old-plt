(module bullet mzscheme 
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred")
           "sig.ss"
           (lib "class100.ss")
           (lib "class.ss"))
  
  (provide bullet@)

  (define bullet@
    (unit/sig bullet^
      (import mred^)
      
      (define bullet-size 
        (make-parameter
         (let ([s (send (send (send (make-object text%) get-style-list) basic-style)
                        get-size)])
           (max 7 (quotient s 2)))))
      
      (define (get-bullet-width)
        (* 2 (bullet-size)))
      
      (define transparent-brush (make-object brush% "WHITE" 'transparent))
      
      (define bullet-snip%
        (class100 snip% (_depth)
          (inherit set-snipclass set-count get-style)
          (private-field [depth _depth])
          (private
            [zero (lambda (b) (when b (set-box! b 0)))]
            [get-height (lambda (dc)
                          (let ([s (get-style)])
                            (max (bullet-size) (- (send s get-text-height dc)
                                                  (send s get-text-descent dc)))))])
          (override
            [get-extent
             (lambda (dc x y wbox hbox descentbox spacebox
                         lspacebox rspacebox)
               (when hbox
                 (set-box! hbox (get-height dc)))
               (when wbox
                 (set-box! wbox (* 2 (bullet-size))))
               (zero descentbox)
               (zero spacebox)
               (zero rspacebox)
               (zero lspacebox))]
            [draw
             (lambda (dc x y . other)
               (let ([y (+ y (ceiling (/ (- (get-height dc) (bullet-size)) 2)))])
                 (let-values ([(draw solid?)
                               (case depth
                                 [(0) (values (lambda (x y w h) (send dc draw-ellipse x y w h)) #t)]
                                 [(1) (values (lambda (x y w h) (send dc draw-ellipse x y w h)) #f)]
                                 [else (values (lambda (x y w h) (send dc draw-rectangle x y w h)) #f)])])
                   (let ([b (send dc get-brush)])
                     (send dc set-brush
                           (if solid?
                               (send the-brush-list
                                     find-or-create-brush
                                     (send (send dc get-pen) get-color)
                                     'solid)
                               transparent-brush))
                     (draw x y (bullet-size) (bullet-size))
                     (send dc set-brush b)))))]
            [copy
             (lambda ()
               (make-object bullet-snip% depth))]
            [write
             (lambda (stream)
               (send stream << depth))]
            [get-text
             (lambda (offset num flattened?)
               (if (< num 1)
                   ""
                   (if flattened?
                       "* "
                       "*")))])
          (sequence
            (super-init)
            (add-snip-class-if-needed)
            (set-snipclass (send (get-the-snip-class-list) find "HTML Bullet"))
            (set-count 1))))
      
      (define bullet-snip-class
        (make-object 
            (class100 snip-class% ()
              (inherit set-classname)
              (override
                [read
                 (lambda (stream)
                   (let ([d-box (box 0)])
                     (send stream >> d-box)
                     (make-object bullet-snip% (unbox d-box))))])
              (sequence
                (super-init)
                (set-classname "HTML Bullet")))))

      (define (add-snip-class-if-needed)
        (unless (send (get-the-snip-class-list) find "HTML Bullet")
          (send (get-the-snip-class-list) add bullet-snip-class))))))

