
(module drawboard mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "list.ss")
           "packicon.ss")

  (provide board-panel%)
  
  (define black (make-object color% 0 0 0))
  
  (define transparent-pen (make-object pen% "white" 1 'transparent))
  
  (define water-brush (make-object brush% "blue" 'solid))
  (define wall-brush (make-object brush% "brown" 'solid))
  (define plain-brush (make-object brush% "gray" 'solid))
  (define base-brush (make-object brush% "dark gray" 'solid))

  (define num-pack-icons 11)
  (define pack-colors (make-package-colors num-pack-icons))

  (define-struct pack (icon pen id dest-x dest-y weight x y))
  (define-struct robot (icon id x y))

  (define max-width 800)
  (define max-height 600)

  (define board-panel%
    (class horizontal-panel%
      (init frame width height 
            ;; board is a vector of row vectors
            board)
      
      (define/public (install-robots orig-robots)
        (send canvas install-robots orig-robots))
      (define/public (install-packages orig-pkgs)
        (send canvas install-packages orig-pkgs))
      
      (super-instantiate (frame))
      (define canvas (make-object board-canvas% this width height board))))
    
  (define board-canvas%
    (class canvas%
      (init frame)
      (init-field width height 
                  ;; board is a vector of row vectors
                  board)

      (define scale (max 1
			 (min (floor (/ max-width width))
			      (floor (/ max-height height))
			      32)))
      (define cell-paint-size (max 1 (sub1 scale)))                               
      
      (define pack-icons (list->vector (make-package-icons cell-paint-size pack-colors)))
      (define pack-arrow-pens (list->vector (map (lambda (color)
                                                   (make-object pen% color 1 'solid))
                                                 pack-colors)))
      
      (define robots null)
      
      (define/public (install-robots orig-robots)
        ;; robot is (list id x y)
        (set! robots
              (map (lambda (orig icon)
                     (make-robot icon
                                 (car orig)
                                 (cadr orig)
                                 (caddr orig)))
                   orig-robots
                   (make-robot-icons cell-paint-size (make-robot-colors (length orig-robots))))))
      
      (define packages null)
      
      (define/public (install-packages orig-pkgs)
        ;; package is (list id x y dest-x dext-y weight)
        (set! packages
              (if (null? orig-pkgs)
                  null
                  ;; Sort by size (smaller first):
                  (let ([pkgs (quicksort orig-pkgs
                                         (lambda (a b)
                                           (<= (cadddr (cddr a)) (cadddr (cddr b)))))])
                    (let ([min (cadddr (cddar pkgs))]
                          [max (cadddr (cddar (last-pair pkgs)))])
                      (map (lambda (pkg)
                             (let ([rel-weight (if (= max min)
                                                   (sub1 num-pack-icons)
                                                   (inexact->exact
                                                    (floor (* (/ (- (cadddr (cddr pkg))
                                                                    min)
                                                                 (- max min))
                                                              (sub1 num-pack-icons)))))])
                               (make-pack (vector-ref pack-icons rel-weight)
                                          (vector-ref pack-arrow-pens rel-weight)
                                          (car pkg)
                                          (cadr (cddr pkg))
                                          (caddr (cddr pkg))
                                          (cadddr (cddr pkg))
                                          (cadr pkg)
                                          (caddr pkg))))
                           pkgs))))))
            
      (define display-w (add1 (* scale width)))
      (define display-h (add1 (* scale height)))

      (inherit get-dc min-client-width min-client-height
               stretchable-width stretchable-height)
	     
      (define offscreen-bm (make-object bitmap% display-w display-h))
      (define offscreen (make-object bitmap-dc% offscreen-bm))

      (send offscreen set-pen transparent-pen)
             
      (define/override (on-paint)
        (send offscreen clear)
        (let loop ([i 0])
          (unless (= i width)
            (let loop ([j 0])
              (unless (= j height)
                (draw-board-pos offscreen i j)
                (loop (add1 j))))
            (loop (add1 i))))
        (for-each (lambda (pack)
                    (draw-package offscreen pack))
                  packages)
        (for-each (lambda (robot)
                    (draw-robot offscreen robot))
                  robots)
        (send (get-dc) draw-bitmap offscreen-bm 0 0))
      
      (define/private (pos->location i j)
        (values (add1 (* i scale)) (add1 (* (- height j 1) scale))))
  
      (define/private (draw-board-pos dc i j)
        (let-values ([(cell) (vector-ref (vector-ref board j) i)]
                     [(x y) (pos->location i j)])
          (send dc set-brush
                (case cell
                  [(water) water-brush]
                  [(wall) wall-brush]
                  [(plain) plain-brush]
                  [(base) base-brush]))
          (send dc draw-rectangle x y cell-paint-size cell-paint-size)))
  
      (define/private (draw-package dc pack)
        (let-values ([(x y) (pos->location (pack-x pack) (pack-y pack))])
          (send dc draw-bitmap (car (pack-icon pack)) x y 
                'solid black 
                (cdr (pack-icon pack)))
          (let-values ([(dest-x dest-y) (pos->location (pack-dest-x pack) (pack-dest-y pack))]
                       [(d) (* scale 7/10)])
            (send dc set-pen (pack-pen pack))
            (send dc draw-line (+ x d) (+ y d) (+ dest-x d) (+ dest-y d))
            (send dc set-pen transparent-pen))))
      
      (define/private (draw-robot dc robot)
        (let-values ([(x y) (pos->location (robot-x robot) (robot-y robot))])
          (send dc draw-bitmap (car (robot-icon robot)) x y 
                'solid black 
                (cdr (robot-icon robot)))))
    
      (super-instantiate (frame))
      (stretchable-width #f)
      (stretchable-height #f)
      (min-client-width display-w)
      (min-client-height display-h))))
