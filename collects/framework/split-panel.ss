(define thumb-canvas%
  (class canvas% (parent)
    
    (private
      [percentage 1/3])
    (public
      [get-percentage (lambda () percentage)]
      [set-percentage (lambda (_p) 
                        (set! percentage _p)
                        (on-paint))])
    (private
      [gray-region 18]
      [canvas-width 18]
      [thumb-height 16])
    
    (inherit get-dc get-client-size)
    (override
      [on-event
       (lambda (evt)
         (when (send evt button-down?)
           (set! percentage (+ percentage 1/10))
           (when (> percentage 1)
             (set! percentage (- percentage 1)))
           (on-paint)
           (send parent recalc)))]
      [on-paint
       (lambda ()
         (let ([dc (get-dc)]
               [panel-color (get-panel-background)])
           (let-values ([(w h) (get-client-size)])
             (send dc set-pen (send the-pen-list find-or-create-pen panel-color 1 'solid))
             (send dc set-brush (send the-brush-list find-or-create-brush panel-color 'solid))
             (send dc draw-rectangle 0 0 gray-region h)
             
             (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'solid))
             (send dc set-brush (send the-brush-list find-or-create-brush "white" 'solid))
             (send dc draw-rectangle gray-region 0 (- w gray-region) h)
             
             (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
             (send dc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
             (let* ([thumb-middle (floor (* h percentage))]
                    [thumb-top (- thumb-middle (/ thumb-height 2))]
                    [thumb-bottom (+ thumb-top thumb-height)])
               (send dc draw-polygon
                     (list (make-object point% 2 thumb-middle)
                           (make-object point% w thumb-top)
                           (make-object point% w thumb-bottom)))))))])
    
    (inherit min-width stretchable-width)
    (sequence 
      (super-init parent)
      (min-width canvas-width)
      (stretchable-width #f))))

(define (resizable-vertical-mixin super%)
  (class super% args
    
    (override
      [container-size
       (lambda (_lst)
         (printf "container-size: ~s~n" container-size)
         ;; remove the thumb canvas from the computation
         (let ([lst (if (null? _lst) null (cdr _lst))])
           (values
            (apply + (map car lst))
            (if (null? lst) 
                0
                (apply max (map cadr lst))))))]
      [place-children
       (lambda (info width height)
         (let* ([percentage (send thumb-canvas get-percentage)]
                [first (floor (* percentage height))]
                [second (- height first)]
                [main-width (- width (send thumb-canvas min-width))])
           (list* (list (- width (send thumb-canvas min-width)) 0
                        (send thumb-canvas min-width)
                        height)
                  (list 0 0 main-width first)
                  (list 0 first main-width second)
                  (map (lambda (x) (list 0 0 0 0)) (cdddr info)))))])
    (inherit reflow-container get-top-level-window)
    (public
      [recalc
       (lambda ()
         (send (get-top-level-window) flush-resize-cache)
         (send (get-top-level-window) reflow-container))]
      [set-percentage
       (lambda (p)
         (send thumb-canvas set-percentage p)
         (reflow-container))])
    
    (sequence
      (apply super-init args))
    (private
      [thumb-canvas (make-object thumb-canvas% this)])))

(define resizable-vertical-panel% (resizable-vertical-mixin panel%))
(define resizable-vertical-pane% (resizable-vertical-mixin pane%))

(define f (make-object frame% "frame" #f 100 400))
(define hp (make-object horizontal-panel% f))
(define rp (make-object resizable-vertical-panel% hp))
(make-object editor-canvas% rp)
(make-object editor-canvas% rp)
(send f show #t)

