
(module drawboard mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "list.ss")
           "packicon.ss"
           (lib "hierlist.ss" "hierlist"))

  (provide board-panel%)
  
  (define black (make-object color% 0 0 0))
  
  (define transparent-pen (make-object pen% "white" 1 'transparent))
  (define red-pen (make-object pen% "red" 1 'solid))
  (define transparent-brush (make-object brush% "white" 'transparent))
  
  (define water-brush (make-object brush% "blue" 'solid))
  (define wall-brush (make-object brush% "brown" 'solid))
  (define plain-brush (make-object brush% "gray" 'solid))
  (define base-brush (make-object brush% "dark gray" 'solid))

  (define bold-style (send (make-object style-delta% 'change-bold) set-delta-background "yellow"))
  (define plain-style (make-object style-delta% 'change-normal))
  
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
        (send canvas install-robots orig-robots robot-list))
      (define/public (install-packages orig-pkgs)
        (send canvas install-packages orig-pkgs package-list))
      
      (super-instantiate (frame))
      (define canvas (make-object board-canvas% this width height board
                       (lambda () (list robot-list package-list))))
      
      (define vpanel (make-object vertical-pane% this))
      (make-object message% "Robots" vpanel)
      (define robot-list (make-object (class hierarchical-list% 
                                        (define/override (on-click i)
                                          (send canvas set-active (send i user-data)))
                                        (super-instantiate ()))
                           vpanel))
      (send robot-list selectable #f)
      (make-object message% "Packages" vpanel)
      (define package-list (make-object (class hierarchical-list% 
                                        (define/override (on-click i)
                                          (send canvas set-active (send i user-data)))
                                        (super-instantiate ()))
                             vpanel))
      (send package-list selectable #f)))
    
  (define board-canvas%
    (class canvas%
      (init frame)
      (init-field width height 
                  ;; board is a vector of row vectors
                  board
                  get-status-lists)

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
      
      (define active-i #f)
      (define active-j #f)
      
      (define/public (install-robots orig-robots hlist)
        ;; robot is (list id x y)
        (set! robots
              (map (lambda (orig icon)
                     (make-robot icon
                                 (car orig)
                                 ;; sub1 for 0-indexed
                                 (sub1 (cadr orig))
                                 (sub1 (caddr orig))))
                   orig-robots
                   (make-robot-icons cell-paint-size (make-robot-colors (length orig-robots)))))
        (map (lambda (i) (send hlist delete-item i)) (send hlist get-items))
        (map (lambda (r) (let* ([i (send hlist new-list)]
                                [e (send i get-editor)])
                           (send e insert (make-object image-snip% (car (robot-icon r))))
                           (send e insert (format " ~a" (robot-id r)))
                           (send i user-data (cons (robot-x r) (robot-y r)))))
             robots))
                           
      
      (define packages null)
      
      (define/public (install-packages orig-pkgs hlist)
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
                                          ;; sub1 for 0-indexed
                                          (sub1 (cadr (cddr pkg)))
                                          (sub1 (caddr (cddr pkg)))
                                          (cadddr (cddr pkg))
                                          (sub1 (cadr pkg))
                                          (sub1 (caddr pkg)))))
                           pkgs)))))
        (map (lambda (i) (send hlist delete-item i)) (send hlist get-items))
        (map (lambda (p) (let* ([i (send hlist new-list)]
                                [e (send i get-editor)])
                           (send e insert (make-object image-snip% (car (pack-icon p))))
                           (send e insert (format " ~a" (pack-id p)))
                           (send i user-data (cons (pack-x p) (pack-y p)))))
             packages))
            
      (define display-w (add1 (* scale width)))
      (define display-h (add1 (* scale height)))

      (inherit get-dc min-client-width min-client-height
               stretchable-width stretchable-height)
	     
      (define offscreen-bm (make-object bitmap% display-w display-h))
      (define offscreen (make-object bitmap-dc% offscreen-bm))

      (send offscreen set-pen transparent-pen)

      (define/override (on-event e)
        (when (send e button-down?)
          (let-values ([(i j) (location->pos (send e get-x) (send e get-y))])
            (set-active (cons i j)))))
      
      (define/public (set-active ij-pair)
        (set! active-i (car ij-pair))
        (set! active-j (cdr ij-pair))
        (on-paint)
        (map (lambda (list)
               (map (lambda (i)
                      (let ([ij-pair (send i user-data)])
                        (send (send i get-editor)
                              change-style
                              (if (and (= active-i (car ij-pair))
                                       (= active-j (cdr ij-pair)))
                                  bold-style
                                  plain-style)
                              0 'end)))
                      (send list get-items)))
             (get-status-lists)))
      
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

        (when active-i
          (let-values ([(x y) (pos->location active-i active-j)])
            (send offscreen set-pen red-pen)
            (send offscreen set-brush transparent-brush)
            (send offscreen draw-rectangle (- x 1) (- y 1) (+ cell-paint-size 2) (+ cell-paint-size 2))
            (send offscreen set-pen transparent-pen)))
        
        (send (get-dc) draw-bitmap offscreen-bm 0 0))
      
      (define/private (pos->location i j)
        (values (add1 (* i scale)) (add1 (* (- height j 1) scale))))
  
      (define/private (location->pos x y)
        (values (min (max 0 (floor (/ (sub1 x) scale))) (sub1 width))
                (- height (min (max 0 (floor (/ (sub1 y) scale))) (sub1 height)) 1)))
  
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
