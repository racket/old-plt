
(module drawboard mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "list.ss")
           "packicon.ss"
           (lib "hierlist.ss" "hierlist"))

  (provide board-panel%)
  
  (define black (make-object color% 0 0 0))
  
  (define transparent-pen (make-object pen% "white" 1 'transparent))
  (define black-pen (make-object pen% "black" 1 'solid))
  (define red-pen (make-object pen% "red" 1 'solid))
  (define transparent-brush (make-object brush% "white" 'transparent))
  (define motion-brush (make-object brush% "black" 'solid))
  
  (define water-brush (make-object brush% "blue" 'solid))
  (define wall-brush (make-object brush% "brown" 'solid))
  (define plain-brush (make-object brush% "gray" 'solid))
  (define base-brush (make-object brush% "dark gray" 'solid))

  (define bold-style (send (make-object style-delta% 'change-bold) set-delta-background "yellow"))
  (define plain-style (make-object style-delta% 'change-normal))
  
  (define num-pack-icons 11)
  (define pack-colors (make-package-colors num-pack-icons))

  (define-struct pack (icon pen id dest-x dest-y weight x y owner))
  (define-struct robot (icon id x y packages motion drop grab dead? moving?))

  (define max-width 800)
  (define max-height 600)

  (define board-panel%
    (class horizontal-panel%
      (init frame width height 
            ;; board is a vector of row vectors
            board)
      
      (define/public (install-robots&packages orig-robots orig-pkgs)
        ;; Each robot is (list id x y (list pkg-id ...))
        ;; Each package is (list id x y dest-x dext-y weight)
        (send canvas install-packages orig-pkgs package-list)
        (send canvas install-robots orig-robots robot-list))
      
      (define/public (queue-robot-actions actions)
        ;; Each robot action is (list id (one-of 'e 'w 'n 's (list 'pick id...) (list 'drop id ...)))
        (send canvas queue-robot-actions actions))
      
      (define/public (apply-queued-actions)
        (send canvas apply-queued-actions))
      
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
    
  (define arrow-inset 1)
  
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
      
      (define actions null)
      
      (define active-i #f)
      (define active-j #f)
      
      (define arrow-height (/ cell-paint-size 8))
      
      (define up-arrow (list (make-object point% (/ cell-paint-size 2) arrow-inset)
                             (make-object point% (- cell-paint-size arrow-inset) (+ arrow-inset arrow-height))
                             (make-object point% arrow-inset (+ arrow-inset arrow-height))))
      (define down-arrow (list (make-object point% (/ cell-paint-size 2) (- cell-paint-size arrow-inset))
                               (make-object point% (- cell-paint-size arrow-inset) (- cell-paint-size arrow-height arrow-inset))
                               (make-object point% arrow-inset (- cell-paint-size arrow-height arrow-inset))))
      (define left-arrow (list (make-object point% arrow-inset (/ cell-paint-size 2))
                               (make-object point% (+ arrow-height arrow-inset) arrow-inset)
                               (make-object point% (+ arrow-height arrow-inset) (- cell-paint-size arrow-inset))))
      (define right-arrow (list (make-object point% (- cell-paint-size arrow-inset) (/ cell-paint-size 2))
                                (make-object point% (- cell-paint-size arrow-height arrow-inset) (- arrow-height arrow-inset))
                                (make-object point% (- cell-paint-size arrow-height arrow-inset) (- cell-paint-size arrow-inset))))
      (define pickup-arrow (list (make-object point% (* cell-paint-size 3/4) (* 2 arrow-inset))
                                 (make-object point% (+ (/ cell-paint-size 2) arrow-inset) (+ (* 2 arrow-inset) (* 2 arrow-height)))
                                 (make-object point% (- cell-paint-size arrow-inset) (+ (* 2 arrow-inset) (* 2 arrow-height)))))
      (define drop-arrow (list (make-object point% (* cell-paint-size 3/4) (+ (* 2 arrow-inset) (* 2 arrow-height)))
                                 (make-object point% (+ (/ cell-paint-size 2) arrow-inset) (* 2 arrow-inset))
                                 (make-object point% (- cell-paint-size arrow-inset) (* 2 arrow-inset))))
      
      (define/public (install-robots orig-robots hlist)
        ;; robot is (list id x y (list pkg-id ...))
        (set! robots
              (map (lambda (orig icon)
                     (let ([pkgs (map (lambda (pid)
                                        (or (ormap (lambda (pkg)
                                                     (and (= (pack-id pkg) pid)
                                                          pkg))
                                                   packages)
                                            (error 'install "robot package not found: ~a" pid)))
                                      (cadddr orig))])
                       (let ([r (make-robot icon
                                            (car orig)
                                            ;; sub1 for 0-indexed
                                            (sub1 (cadr orig))
                                            (sub1 (caddr orig))
                                            pkgs
                                            #f
                                            #f
                                            #f
                                            #f
                                            #f)])
                         (for-each (lambda (pkg) 
                                     (when (pack-owner pkg) 
                                       (error 'install
                                              "two robots own package ~a" (pack-id pkg)))
                                     (unless (and (= (pack-x pkg) (robot-x r))
                                                  (= (pack-y pkg) (robot-y r)))
                                       (error 'install
                                              "owning robot ~a not on the same location as owned package ~a" 
                                              (robot-id r)
                                              (pack-id pkg)))
                                     (set-pack-owner! pkg r))
                                   pkgs)
                         r)))
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
        ;; Each package is (list id x y dest-x dext-y weight)
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
                                          (sub1 (caddr pkg))
                                          #f)))
                           pkgs)))))
        (map (lambda (i) (send hlist delete-item i)) (send hlist get-items))
        (map (lambda (p) (let* ([i (send hlist new-list)]
                                [e (send i get-editor)])
                           (send e insert (make-object image-snip% (car (pack-icon p))))
                           (send e insert (format " ~a" (pack-id p)))
                           (send i user-data (cons (pack-x p) (pack-y p)))))
             packages))
      
      (define/public (queue-robot-actions orig-actions)
        (set! actions
              (map (lambda (act)
                     (let ([id (car act)])
                       (let* ([r (ormap (lambda (r) (and (= (robot-id r) id) r)) robots)]
                              [get-pkgs (lambda (l)
                                          (unless (and (list? l)
                                                       (andmap number? l))
                                            (error 'queue-robot-actions
                                                   "bad action ~e"
                                                   (cadr act)))
                                          (map (lambda (pid)
                                                 (let ([pkg (ormap (lambda (pkg)
                                                                     (and (= (pack-id pkg) pid)
                                                                          pkg))
                                                                   packages)])
                                                   (unless pkg
                                                     (error 'queue-robot-actions
                                                            "can't find package ~e"
                                                            pid))
                                                   pkg))
                                               l))])
                         (unless r
                           (error 'queue-robot-actions "cannot find robot ~e" id))
                         (when (or (robot-motion r)
                                   (robot-grab r)
                                   (robot-drop r))
                           (error 'queue-robot-actions "robot ~e has an action already" id))
                         (cond
                           [(and (pair? (cadr act)) (eq? (caadr act) 'pick))
                            (set-robot-motion! r pickup-arrow)
                            (set-robot-grab! r (get-pkgs (cdadr act)))]
                           [(and (pair? (cadr act)) (eq? (caadr act) 'drop))
                            (set-robot-motion! r drop-arrow)
                            (set-robot-drop! r (get-pkgs (cdadr act)))]
                           [else
                            (set-robot-motion! r
                                               (case (cadr act)
                                                 [(e) right-arrow]
                                                 [(w) left-arrow]
                                                 [(n) up-arrow]
                                                 [(s) down-arrow]
                                                 [else (error 'queue-robot-actions
                                                              "bad action ~e"
                                                              (cadr act))]))])
                         r)))
                   orig-actions))
        (on-paint))
      
      (define/private (find-robot x y)
        (ormap (lambda (r)
                 (and (= x (robot-x r))
                      (= y (robot-y r))
                      (not (robot-moving? r))
                      r))
               robots))
      
      (define animate-steps 5)
      (define/private (animate f)
        (let loop ([i 1])
          (unless (> i animate-steps)
            (let ([continue? (f i)])
              (sleep/yield 0.1)
              (on-paint)
              (when continue?
                (loop (add1 i)))))))
      
      (define/public (apply-queued-actions)
        (for-each (lambda (r)
                    (animate
                     (let loop ([r r][motion (robot-motion r)])
                       (set-robot-motion! r #f)
                       (set-robot-moving?! r #t)
                       (let-values ([(dx dy)
                                     (cond
                                       [(eq? motion left-arrow) (values -1 0)]
                                       [(eq? motion right-arrow) (values 1 0)]
                                       [(eq? motion up-arrow) (values 0 1)]
                                       [(eq? motion down-arrow) (values 0 -1)]
                                       [else (values 0 0)])])
                         (let ([nx (+ (robot-x r) dx)]
                               [ny (+ (robot-y r) dy)])
                           (let* ([push-r (and (or (not (zero? dx))
                                                   (not (zero? dy)))
                                               (find-robot nx ny))]
                                  [sub-animate
                                   (if push-r
                                       (begin
                                         ;; "Reboot" the pushed robot, and push it the
                                         ;;  same as this one:
                                         (set-robot-motion! r #f)
                                         (set-robot-drop! r #f)
                                         (set-robot-grab! r #f)
                                         (loop push-r motion)) ;; returns an animation step
                                       (lambda (i) #f))]) ;; #f "means nothing to animate"
                             (let* ([on-board? (and (< -1 nx width)
                                                    (< -1 ny height))]
                                    [cell (if (and push-r (find-robot nx ny)) ;; still a robot there?
                                              'wall
                                              (if on-board?
                                                  (pos->cell nx ny)
                                                  'wall))]
                                    [drop/grab (lambda (r)
                                                 (when (robot-grab r)
                                                   (for-each (lambda (pkg)
                                                               (when (not (pack-owner pkg))
                                                                 (set-pack-owner! pkg r)
                                                                 (set-robot-packages! r (cons pkg (robot-packages r)))))
                                                             (robot-grab r))
                                                   (set-robot-grab! r #f))
                                                 (when (robot-drop r)
                                                   (for-each (lambda (pkg)
                                                               (when (eq? r (pack-owner pkg))
                                                                 (set-pack-owner! pkg #f)
                                                                 (set-robot-packages! r (remq pkg (robot-packages r)))))
                                                             (robot-drop r))
                                                   (set-robot-drop! r #f)))])
                            (if (or (and (zero? dx) (zero? dy)) (eq? cell 'wall))
                                (begin
                                  (set-robot-moving?! r #f)
                                  (lambda (i)
                                    (drop/grab r)
                                    (sub-animate i)))
                                (begin
                                  (when (eq? cell 'water)
                                    (set-robot-dead?! r #t))
                                  (let ([x (robot-x r)]
                                        [y (robot-y r)])
                                    (lambda (i)
                                      (drop/grab r)
                                      (sub-animate i)
                                      (set-robot-moving?! r #f)
                                      (let ([nx (+ x (* (- nx x) (/ i animate-steps)))]
                                            [ny (+ y (* (- ny y) (/ i animate-steps)))])
                                        (set-robot-x! r nx)
                                        (set-robot-y! r ny)
                                        (for-each (lambda (pkg)
                                                    (set-pack-x! pkg nx)
                                                    (set-pack-y! pkg nx))
                                                  (robot-packages r)))
                                      #t)))))))))))
                  actions)
        (set! actions null)
        (on-paint))

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
        (let ([non-water-rgn (make-object region% offscreen)])
          (send non-water-rgn set-rectangle 0 0 display-w display-h)
          (send offscreen clear)
          (let loop ([i 0])
            (unless (= i width)
              (let loop ([j 0])
                (unless (= j height)
                  (draw-board-pos offscreen i j non-water-rgn)
                  (loop (add1 j))))
              (loop (add1 i))))
          (send offscreen set-clipping-region non-water-rgn)
          (for-each (lambda (robot)
                      (draw-robot offscreen robot))
                    robots)
          (send offscreen set-clipping-region #f)
          (for-each (lambda (pack)
                      (draw-package offscreen pack))
                    packages)
          (when active-i
            (let-values ([(x y) (pos->location active-i active-j)])
              (send offscreen set-pen red-pen)
              (send offscreen set-brush transparent-brush)
              (send offscreen draw-rectangle (- x 1) (- y 1) (+ cell-paint-size 2) (+ cell-paint-size 2))
              (send offscreen set-pen transparent-pen)))
        
          (send (get-dc) draw-bitmap offscreen-bm 0 0)))
      
      (define/private (pos->location i j)
        (values (add1 (* i scale)) (add1 (* (- height j 1) scale))))
  
      (define/private (location->pos x y)
        (values (min (max 0 (floor (/ (sub1 x) scale))) (sub1 width))
                (- height (min (max 0 (floor (/ (sub1 y) scale))) (sub1 height)) 1)))
  
      (define (pos->cell i j)
        (vector-ref (vector-ref board j) i))
                        
      (define/private (draw-board-pos dc i j non-water-rgn)
        (let-values ([(cell) (pos->cell i j)]
                     [(x y) (pos->location i j)])
          (send dc set-brush
                (case cell
                  [(water) 
                   (let ([water-rgn (make-object region% dc)])
                     (send water-rgn set-rectangle x y cell-paint-size cell-paint-size)
                     (send non-water-rgn subtract water-rgn))
                   water-brush]
                  [(wall) wall-brush]
                  [(plain) plain-brush]
                  [(base) base-brush]))
          (send dc draw-rectangle x y cell-paint-size cell-paint-size)))
  
      (define margin 2)
      
      (define/private (draw-package dc pack)
        (let*-values ([(x y) (pos->location (pack-x pack) (pack-y pack))]
                      [(icon) (car (pack-icon pack))]
                      [(dy) (- cell-paint-size (send icon get-height) margin)]
                      [(dx) (if (pack-owner pack)
                                margin
                                (- cell-paint-size (send icon get-width) margin))])
          (send dc draw-bitmap icon (+ dx x) (+ y dy)
                'solid black 
                (cdr (pack-icon pack)))
          (let-values ([(dest-x dest-y) (pos->location (pack-dest-x pack) (pack-dest-y pack))]
                       [(ddx) (/ (send icon get-width) 2)]
                       [(ddy) (/ (send icon get-height) 2)])
            (send dc set-pen (pack-pen pack))
            (send dc draw-line (+ x ddx dx) (+ y ddy dy) (+ dest-x dx ddx) (+ dest-y dy ddy))
            (send dc set-pen transparent-pen))))
      
      (define/private (draw-robot dc robot)
        (let-values ([(x y) (pos->location (robot-x robot) (robot-y robot))])
          (when (robot-motion robot)
            (send dc set-brush motion-brush)
            (send dc set-pen black-pen)
            (send dc draw-polygon (robot-motion robot) x y)
            (send dc set-pen transparent-pen))
          (send dc draw-bitmap (car (robot-icon robot)) (+ 1 x) y 
                'solid black 
                (cdr (robot-icon robot)))))
            
      (super-instantiate (frame))
      (stretchable-width #f)
      (stretchable-height #f)
      (min-client-width display-w)
      (min-client-height display-h))))
