
(module robotclient mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "unit.ss")
           (lib "list.ss")
           "packicon.ss")

  (define black (make-object color% 0 0 0))
  
  (define transparent-pen (make-object pen% "white" 1 'transparent))
  
  (define water-brush (make-object brush% "blue" 'solid))
  (define wall-brush (make-object brush% "brown" 'solid))
  (define plain-brush (make-object brush% "gray" 'solid))
  (define base-brush (make-object brush% "dark gray" 'solid))

  (define num-pack-icons 11)

  (define-struct pack (icon pen id dest-x dest-y weight x y done?))
  (define-struct robot (icon id x y))
  
  (define u
    (unit 
      (import width height board orig-pkgs orig-robots)
      (export)

      (define max-width 800)
      (define max-height 600)

      (define scale (max 1
			 (min (floor (/ max-width width))
			      (floor (/ max-height height))
			      32)))
      (define cell-paint-size (max 1 (sub1 scale)))                               

      (define pack-colors (make-package-colors num-pack-icons))
      (define pack-icons (list->vector (make-package-icons cell-paint-size pack-colors)))
      (define pack-arrow-pens (list->vector (map (lambda (color)
                                                   (make-object pen% color 1 'solid))
                                                 pack-colors)))
      
      (define robots (map (lambda (orig icon)
                            (make-robot icon
                                        (car orig)
                                        (cadr orig)
                                        (caddr orig)))
                          orig-robots
                          (make-robot-icons cell-paint-size (make-robot-colors (length orig-robots)))))

      (define-values (base-x base-y)
        (let iloop ([i 0])
          (when (= i width) (error "can't find base"))
          (let loop ([j 0])
            (cond
              [(= j height)
               (iloop (add1 i))]
              [(eq? 'base (vector-ref (vector-ref board i) j))
               (values i j)]
              [else (loop (add1 j))]))))
      
      (define packages
        (if (null? orig-pkgs)
            null
            ;; Sort by size (smaller first):
            (let ([pkgs (quicksort orig-pkgs
                                   (lambda (a b)
                                     (<= (cadddr a) (cadddr b))))])
              (let ([min (cadddr (car pkgs))]
                    [max (cadddr (car (last-pair pkgs)))])
                (map (lambda (pkg)
                       (let ([rel-weight (if (= max min)
                                             (sub1 num-pack-icons)
                                             (inexact->exact
                                              (floor (* (/ (- (cadddr pkg)
                                                              min)
                                                           (- max min))
                                                        (sub1 num-pack-icons)))))])
                         (make-pack (vector-ref pack-icons rel-weight)
                                    (vector-ref pack-arrow-pens rel-weight)
                                    (car pkg)
                                    (cadr pkg)
                                    (caddr pkg)
                                    (cadddr pkg)
                                    base-x
                                    base-y
                                    #f)))
                     pkgs)))))
            
      (define f	(instantiate frame% ("Robot") [style '(no-resize-border)]))
      (define canvas
	(let ([display-w (* scale width)]
	      [display-h (* scale height)])
	  (instantiate
	   (class canvas%
	     (inherit get-dc min-client-width min-client-height)
	     
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
	     
	     (super-instantiate (f))
             (min-client-width display-w)
             (min-client-height display-h))
            ())))
      
      (define (pos->location i j)
        (values (* i scale) (* (- height j 1) scale)))

      (define (draw-board-pos dc i j)
	(let-values ([(cell) (vector-ref (vector-ref board i) j)]
                     [(x y) (pos->location i j)])
	  (send dc set-brush
		(case cell
		  [(water) water-brush]
		  [(wall) wall-brush]
		  [(plain) plain-brush]
		  [(base) base-brush]))
	  (send dc draw-rectangle x y cell-paint-size cell-paint-size)))
	  
      (define (draw-package dc pack)
        (unless (pack-done? pack)
          (let-values ([(x y) (pos->location (pack-x pack) (pack-y pack))])
            (send dc draw-bitmap (pack-icon pack) x y 
                  'solid black 
                  (send (pack-icon pack) get-loaded-mask))
            (let-values ([(dest-x dest-y) (pos->location (pack-dest-x pack) (pack-dest-y pack))]
                         [(d) (* scale 7/10)])
              (send dc set-pen (pack-pen pack))
              (send dc draw-line (+ x d) (+ y d) (+ dest-x d) (+ dest-y d))
              (send dc set-pen transparent-pen)))))
              
       (define (draw-robot dc robot)
         (let-values ([(x y) (pos->location (robot-x robot) (robot-y robot))])
           (send dc draw-bitmap (robot-icon robot) x y 
                 'solid black 
                 (send (robot-icon robot) get-loaded-mask))))
       
       (send f show #t)))

  (invoke-unit u 10 10 #(#(water water water water water water water water water water)
                          #(plain plain plain plain plain plain plain plain plain plain)
                          #(plain plain plain plain plain plain plain plain plain plain)
                          #(plain plain plain plain plain plain plain plain plain plain)
                          #(plain plain plain plain plain plain plain plain plain plain)
                          #(wall  wall  plain plain plain plain plain plain plain plain)
                          #(plain plain plain plain plain plain plain plain plain plain)
                          #(plain plain plain plain plain plain plain plain base  plain)
                          #(plain plain plain plain plain plain plain plain plain plain)
                          #(plain plain plain plain plain plain plain plain plain plain)
                          #(plain plain plain plain plain plain plain plain plain plain))
               '((3 8 8 20)
                 (1 5 5 100))
               '((1 2 2)
                 (2 7 4)
                 (3 4 4)
                 (4 2 1)
                 (5 9 2)
                 (6 9 3)
                 (7 6 2)
                 (8 6 3))))
      