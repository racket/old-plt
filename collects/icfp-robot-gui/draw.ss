
(module draw mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "hierlist.ss" "hierlist")
           "icon.ss"
           "data.ss")

  (provide board-panel%)
  
  (define animate? #t)
  (define animate-steps 2)
  
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
  (define unknown-pack-color (make-unknown-package-color))
  
  (define-struct thing (x y))

  (define-struct (pack thing) (icon pen id dest-x dest-y weight owner home?))
  (define-struct (robot thing) (icon dead-icon id packages money max-lift motion drop grab dead? moving? 
                                     bid bid-index score pos activity))

  (define robot-x thing-x)
  (define robot-y thing-y)
  (define pack-x thing-x)
  (define pack-y thing-y)
  (define set-robot-x! set-thing-x!)
  (define set-robot-y! set-thing-y!)
  (define set-pack-x! set-thing-x!)
  (define set-pack-y! set-thing-y!)
  
  (define max-width 800)
  (define max-height 600)
  
  (define (fill item label value)
    (let ([e (send item get-editor)])
      (send e erase)
      (send e insert (format "~a: ~a" label value))))

  (define (set-robot-info r item)
    (let ([sub-items (send item get-items)])
      (fill (car sub-items) "Score" (robot-score r))
      (fill (cadr sub-items) "Money" (robot-money r))
      (fill (caddr sub-items) "Bid" (robot-bid r))
      (fill (cadddr sub-items) "Capacity" (robot-max-lift r))
      (fill (cadddr (cdr sub-items)) "Load" (apply + (map pack-weight (robot-packages r))))))
  
  (define (set-pack-info p item)
    (let ([sub-items (send item get-items)])
      (fill (car sub-items) "Weight" (or (pack-weight p) 'unknown))
      (fill (cadr sub-items) "Owner" (if (pack-owner p) 
                                         (robot-id (pack-owner p))
                                         'none))))
  
  (define board-panel%
    (class horizontal-panel%
      (init frame width height 
            ;; board is a vector of row vectors
            board)
      
      (define/public (install-robots&packages orig-robots orig-pkgs)
        ;; Each robot is (list id x y money max-lift (list pkg-id ...))
        ;; Each package is (list id x y dest-x dext-y weight)
        (send canvas install-packages orig-pkgs package-list)
        (send canvas install-robots orig-robots robot-list))
      
      (define/public (queue-robot-actions actions)
        ;; Each robot action is (list id bid (one-of 'e 'w 'n 's (list 'pick id...) (list 'drop id ...)))
        (send canvas queue-robot-actions actions))
      
      (define/public (apply-queued-actions)
        (send canvas apply-queued-actions))
      
      (define/public (get-most-recent-activity)
        (send canvas get-most-recent-activity))

      (define/public (get-robots&packages)
        (send canvas get-robots&packages))
      
      (define/public (get-dead-robot-scores)
        (send canvas get-dead-robot-scores))

      (super-instantiate (frame))
      (define canvas (make-object board-canvas% this width height board
                       (lambda () (list robot-list package-list))))
      
      (define hier% (class hierarchical-list% 
                      (define/override (on-click i)
                        (send canvas set-active 
                              (thing-x (send i user-data))
                              (thing-y (send i user-data))))
                      (super-instantiate ())))
      
      (define vpanel (make-object vertical-pane% this))
      (make-object message% "Robots" vpanel)
      (define robot-list (make-object hier% vpanel))
      (send robot-list selectable #f)
      (make-object message% "Packages" vpanel)
      (define package-list (make-object hier% vpanel))
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
      (define unknown-pack-icon (car (make-package-icons cell-paint-size (list unknown-pack-color))))
      
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
        ;; Each robot is (list id x y money max-lift (list pkg-id ...))
        (define rpos 0)
        (set! robots
              (map (lambda (orig icons)
                     (let ([pkgs (map (lambda (pid)
                                        (or (ormap (lambda (pkg)
                                                     (and (= (pack-id pkg) pid)
                                                          pkg))
                                                   packages)
                                            (error 'install "robot package not found: ~a" pid)))
                                      (bot-packages orig))])
                       (let ([r (make-robot (sub1 (bot-x orig)) ; sub1 for 0-indexed
                                            (sub1 (bot-y orig))
                                            (car icons)
                                            (cdr icons)
                                            (bot-id orig)
                                            pkgs
                                            (bot-money orig)
                                            (bot-max-lift orig)
                                            #f
                                            #f
                                            #f
                                            #f
                                            #f
                                            0       ; pending bid
                                            #f      ; bid index
                                            0       ; score
                                            rpos    ; pos
                                            null)]) ; activity
                         (set! rpos (add1 rpos))
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
                           (send i user-data r)
                           (send i new-item)
                           (send i new-item)
                           (send i new-item)
                           (send i new-item)
                           (send i new-item)
                           (set-robot-info r i)))
             robots)
        (update))
      
      (define packages null)
      
      (define/public (install-packages orig-pkgs hlist)
        ;; Each package is (list id x y dest-x dext-y weight)
        (set! packages
              (if (null? orig-pkgs)
                  null
                  ;; Sort by size (smaller first, unknown at end):
                  (let ([pkgs (quicksort orig-pkgs
                                         (lambda (a b)
                                           (let ([aw (pkg-weight a)]
                                                 [bw (pkg-weight b)])
                                             (cond
                                               [(not aw) #f]
                                               [(not bw) #t]
                                               [else (<= aw bw)]))))])
                    (let* ([min (pkg-weight (car pkgs))]
                           [max (let loop ([l pkgs][mx min])
                                  (if (null? l)
                                      mx
                                      (let ([wt (pkg-weight (car l))])
                                        (loop (cdr l) (if wt (max mx wt) mx)))))])
                      (map (lambda (pkg)
                             (let* ([weight (pkg-weight pkg)]
                                    [rel-weight (cond
                                                  [(not weight) 0]
                                                  [(= max min)
                                                   (sub1 num-pack-icons)]
                                                  [else
                                                   (inexact->exact
                                                    (floor (* (/ (- weight
                                                                    min)
                                                                 (- max min))
                                                              (sub1 num-pack-icons))))])]
                                   [dest-x (pkg-dest-x pkg)]
                                   [dest-y (pkg-dest-y pkg)])
                               (make-pack (sub1 (pkg-x pkg)) ; sub1 for 0-indexed
                                          (sub1 (pkg-y pkg))
                                          (if weight
                                              (vector-ref pack-icons rel-weight)
                                              unknown-pack-icon)
                                          (vector-ref pack-arrow-pens rel-weight)
                                          (pkg-id pkg)
                                          (and dest-x (sub1 dest-x))
                                          (and dest-y (sub1 dest-y))
                                          weight
                                          #f
                                          #f)))
                           pkgs)))))
        (map (lambda (i) (send hlist delete-item i)) (send hlist get-items))
        (map (lambda (p) (let* ([i (send hlist new-list)]
                                [e (send i get-editor)])
                           (send e insert (make-object image-snip% (car (pack-icon p))))
                           (send e insert (format " ~a" (pack-id p)))
                           (send i user-data p)
                           (send i new-item)
                           (send i new-item)
                           (set-pack-info p i)))
             packages))
      
      (define/public (queue-robot-actions orig-actions)
        ;; Each robot action is (list id bid (one-of 'e 'w 'n 's (list 'pick id...) (list 'drop id ...)))
        (define bid-index 0)
        (map (lambda (r) (set-robot-bid-index! r #f)) robots)
        (set! actions
              (map (lambda (act)
                     (let ([id (car act)])
                       (let* ([r (ormap (lambda (r) (and (= (robot-id r) id) r)) robots)]
                              [get-pkgs (lambda (l)
                                          (unless (and (list? l)
                                                       (andmap number? l))
                                            (error 'queue-robot-actions
                                                   "bad action ~e"
                                                   (cadddr act)))
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
                                               l))]
                              [mot (caddr act)])
                         (unless r
                           (error 'queue-robot-actions "cannot find robot ~e" id))
                         (when (robot-dead? r)
                           (error 'queue-robot-actions "dead robot can't act: ~e" id))
                         (when (or (robot-motion r)
                                   (robot-grab r)
                                   (robot-drop r))
                           (error 'queue-robot-actions "robot ~e has an action already" id))
                         (set-robot-bid-index! r bid-index)
                         (set! bid-index (add1 bid-index))
                         (set-robot-bid! r (abs (cadr act)))
                         (cond
                           [(and (pair? mot) (eq? (car mot) 'pick))
                            (set-robot-motion! r pickup-arrow)
                            (set-robot-grab! r (get-pkgs (cdr mot)))]
                           [(and (pair? mot) (eq? (car mot) 'drop))
                            (set-robot-motion! r drop-arrow)
                            (set-robot-drop! r (get-pkgs (cdr mot)))]
                           [else
                            (set-robot-motion! r
                                               (case mot
                                                 [(e) right-arrow]
                                                 [(w) left-arrow]
                                                 [(n) up-arrow]
                                                 [(s) down-arrow]
                                                 [else (error 'queue-robot-actions
                                                              "bad action ~e"
                                                              mot)]))])
                         r)))
                   orig-actions))
        (update))
      
      (define/private (find-robots x y)
        (let ([l (filter robot?
                         (map (lambda (r)
                                (and (= x (robot-x r))
                                     (= y (robot-y r))
                                     (not (robot-moving? r))
                                     (not (robot-dead? r))
                                     r))
                              robots))])
          (if (null? l)
              #f
              l)))
      
      (define/private (animate f)
        (when f
          (if animate?
              (let loop ([i 1])
                (unless (> i animate-steps)
                  (let ([continue? (f i (= i animate-steps))])
                    (sleep/yield 0.05)
                    (update #f)
                    (when continue?
                      (loop (add1 i))))))
              (f animate-steps #t))))
      
      (define/public (apply-queued-actions)
        ;; Forget old activity:
        (for-each (lambda (r)
                    (set-robot-activity! r null))
                  robots)
        ;; Clear dead robots and home packages, first:
        (when (ormap robot-dead? robots)
          (set! robots (filter (lambda (r) (not (robot-dead? r))) robots)))
        (when (ormap pack-home? packages)
          (set! packages (filter (lambda (r) (not (pack-home? r))) packages)))
        ;; Terminate over-bidders:
        (when (ormap (lambda (r)
                       (and (> (robot-bid r) (robot-money r))
                            (set-robot-dead?! r #t)
                            (set-robot-motion! r #f)
                            (set-robot-drop! r #f)
                            (set-robot-grab! r #f)))
                     robots)
          (update))
        ;; Run actions:
        (for-each (lambda (r)
                    (animate
                     (let loop ([r r][motion (robot-motion r)][pushed? #f])
                       (set-robot-motion! r #f)
                       (set-robot-moving?! r #t)
                       (let-values ([(dx dy potential-activity)
                                     (cond
                                       [(eq? motion left-arrow) (values -1 0 '|W|)]
                                       [(eq? motion right-arrow) (values 1 0 '|E|)]
                                       [(eq? motion up-arrow) (values 0 1 '|N|)]
                                       [(eq? motion down-arrow) (values 0 -1 '|S|)]
                                       [else (values 0 0 #f)])])
                         (let ([nx (+ (robot-x r) dx)]
                               [ny (+ (robot-y r) dy)])
                           (let* ([push-rs (and (or (not (zero? dx))
                                                    (not (zero? dy)))
                                                (find-robots nx ny))]
                                  [sub-animate
                                   (if push-rs
                                       (let ([procs
                                              (filter
                                               procedure?
                                               (map
                                                (lambda (push-r)
                                                  ;; "Reboot" the pushed robot, and push it the
                                                  ;;  same as this one:
                                                  (set-robot-motion! r #f)
                                                  (set-robot-drop! r #f)
                                                  (set-robot-grab! r #f)
                                                  (loop push-r motion #t)) ;; returns an animation step
                                                push-rs))])
                                         (if (null? procs)
                                             #f
                                             (lambda (i last?)
                                               (map (lambda (p) (p i last?)) procs))))
                                       (lambda (i last?) #f))]) ;; #f "means nothing to animate"
                             (let* ([on-board? (and (< -1 nx width)
                                                    (< -1 ny height))]
                                    [cell (if (and push-rs (find-robots nx ny)) ;; still a robot there?
                                              'wall
                                              (if on-board?
                                                  (pos->cell nx ny)
                                                  'wall))]
                                    [bid (lambda (r)
                                           (unless pushed?
                                             (set-robot-money! r (max 0 (- (robot-money r) (robot-bid r))))
                                             (set-robot-bid! r 0)))]
                                    [drop/grab (lambda (r random-drop)
                                                 ;; Returns a list of pkgs dropped b/c the robot is dead
                                                 (let ([drop-by-death (and (robot-dead? r)
                                                                           (pair? (robot-packages r))
                                                                           (robot-packages r))])
                                                   (when (robot-grab r)
                                                     (for-each (lambda (pkg)
                                                                 (when (and (not (pack-owner pkg))
                                                                            (not (pack-home? pkg))
                                                                            (= (pack-x pkg) (robot-x r))
                                                                            (= (pack-y pkg) (robot-y r))
                                                                            (>= (robot-max-lift r) 
                                                                                (+ (pack-weight pkg)
                                                                                   (apply
                                                                                    +
                                                                                    (map pack-weight (robot-packages r))))))
                                                                   (set-robot-activity! r
                                                                                        (cons `(|P| ,(pack-id pkg))
                                                                                              (robot-activity r)))
                                                                   (set-pack-owner! pkg r)
                                                                   (set-robot-packages! r (cons pkg (robot-packages r)))))
                                                               (robot-grab r))
                                                     (set-robot-grab! r #f))
                                                   (when (or (robot-drop r) random-drop drop-by-death)
                                                     (for-each (lambda (pkg)
                                                                 (when (eq? r (pack-owner pkg))
                                                                   (when (and (pack-dest-x pkg)
                                                                              (= (pack-x pkg) (pack-dest-x pkg))
                                                                              (pack-dest-y pkg)
                                                                              (= (pack-y pkg) (pack-dest-y pkg)))
                                                                     (set-pack-home?! pkg #t)
                                                                     (set-robot-score! r (+ (robot-score r)
                                                                                            (pack-weight pkg))))
                                                                   (set-robot-activity! r
                                                                                        (cons `(|D| ,(pack-id pkg))
                                                                                              (robot-activity r)))
                                                                   (set-pack-owner! pkg #f)
                                                                   (set-robot-packages! r (remq pkg (robot-packages r)))))
                                                               (append (or (robot-drop r) null)
                                                                       (or random-drop null)
                                                                       (or drop-by-death null))))
                                                   (set-robot-drop! r #f)
                                                   drop-by-death))]
                                    [random-drop (and pushed?
                                                      (not (null? (robot-packages r)))
                                                      (list (list-ref (robot-packages r) 
                                                                      (random (length (robot-packages r))))))])
                               (if (or (and (zero? dx) (zero? dy))
                                       (eq? cell 'wall))
                                   (begin
                                     (set-robot-moving?! r #f)
                                     (if (or (positive? (robot-bid r))
                                             (robot-grab r)
                                             (robot-drop r))
                                         (lambda (i last?)
                                           (bid r)
                                           (drop/grab r random-drop)
                                           (when sub-animate
                                             (sub-animate i last?)))
                                         #f)) ;; nothing at all to do
                                   (begin
                                     (let ([x (robot-x r)]
                                           [y (robot-y r)])
                                       (set-robot-activity! r (cons potential-activity (robot-activity r)))
                                       (lambda (i last?)
                                         (when last?
                                           (when (eq? cell 'water)
                                             (set-robot-dead?! r #t)))
                                         (bid r)
                                         (let ([dead-drop (drop/grab r random-drop)])
                                           (sub-animate i last?)
                                           (set-robot-moving?! r #f)
                                           (let ([nx (+ x (* (- nx x) (/ i animate-steps)))]
                                                 [ny (+ y (* (- ny y) (/ i animate-steps)))])
                                             (set-robot-x! r nx)
                                             (set-robot-y! r ny)
                                             (for-each (lambda (pkg)
                                                         (set-pack-x! pkg nx)
                                                         (set-pack-y! pkg ny))
                                                       (append (robot-packages r)
                                                               ;; Need to move anything we may have dropped
                                                               ;;  while moving into water
                                                               (if (and dead-drop (eq? cell 'water))
                                                                   dead-drop
                                                                   null)))))
                                         #t)))))))))))
                  actions)
        (set! actions null)
        (update))
      
      (define/public (get-most-recent-activity)
        ;; Each activity is (list id action ...)
        ;;  where each action is 'E, 'W, 'N, 'S, '(D id), or '(P id)
        (map (lambda (r)
               (cons (robot-id r)
                     (reverse (robot-activity r))))
             robots))
      
      (define/public (get-robots&packages)
        (values (map (lambda (r)
                       (list (robot-id r)
                             (add1 (robot-x r))
                             (add1 (robot-y r))
                             (robot-money r)
                             (robot-max-lift r)
                             (map pack-id (robot-packages r))))
                     (filter (lambda (r) (not (robot-dead? r))) robots))
                (map (lambda (p)
                       (list (pack-id p)
                             (add1 (pack-x p))
                             (add1 (pack-y p))
                             (add1 (pack-dest-x p))
                             (add1 (pack-dest-y p))
                             (pack-weight p)))
                     (filter (lambda (p) (not (pack-home? p))) packages))))
      
      (define/public (get-dead-robot-scores)
        (map (lambda (r) (list (robot-id r) (robot-score r)))
             (filter robot-dead? robots)))

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
            (set-active i j))))
      
      (define/public (set-active i j)
        (set! active-i i)
        (set! active-j j)
        (update)
        (map (lambda (list)
               (map (lambda (i)
                      (let ([thing (send i user-data)])
                        (send (send i get-editor)
                              change-style
                              (if (and (= active-i (thing-x thing))
                                       (= active-j (thing-y thing)))
                                  bold-style
                                  plain-style)
                              0 'end)))
                      (send list get-items)))
             (get-status-lists)))
      
      (define non-water-rgn (make-object region% offscreen))
      
      (define/private update
        (opt-lambda ([lists-too? #t])
          ;; Update diplay:
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
          (on-paint)
          
          ;; Update lists:
          (when lists-too?
            (let-values ([(robot-list pack-list) (apply values (get-status-lists))])
              (send (send robot-list get-editor) begin-edit-sequence)
              (send robot-list sort (lambda (a b)
                                      ;; Put dead robots at end, in score order.
                                      ;; Put active robots at beginning, in bid order
                                      ;; Put other robots in between, in score order
                                      (let ([a (send a user-data)]
                                            [b (send b user-data)]
                                            [by-score/pos (lambda (a b)
                                                            (if (= (robot-score a) (robot-score b))
                                                                (< (robot-pos a) (robot-pos b))
                                                                (> (robot-score a) (robot-score b))))])
                                        (cond
                                          [(or (not a) (not b)) #t]
                                          [(and (robot-dead? a) (robot-dead? b))
                                           (by-score/pos a b)]
                                          [(robot-dead? a)
                                           #f]
                                          [(robot-dead? b)
                                           #t]
                                          [(and (robot-bid-index a) (robot-bid-index b))
                                           (< (robot-bid-index a) (robot-bid-index b))]
                                          [(robot-bid-index a)
                                           #t]
                                          [(robot-bid-index b)
                                           #f]
                                          [else (by-score/pos a b)]))))
              (let loop ([items (send robot-list get-items)]
                         [pos 0])
                (unless (null? items)
                  (let ([r (send (car items) user-data)])
                    (when (and (robot-dead? r)
                               (not (eq? 'really (robot-dead? r))))
                      (let ([e (send (car items) get-editor)])
                        (send e delete 0 1)
                        (send e set-position 0)
                        (send e insert (make-object image-snip% (car (robot-dead-icon r)))))
                      (set-robot-dead?! r 'really))
                    (set-robot-pos! r pos)
                    (set-robot-info r (car items)))
                  (loop (cdr items) (add1 pos))))
              (send (send robot-list get-editor) end-edit-sequence)
              (send (send pack-list get-editor) begin-edit-sequence)
              (for-each (lambda (i)
                          (set-pack-info (send i user-data) i))
                        (send pack-list get-items))
              (send (send pack-list get-editor) end-edit-sequence)))))
          
      (define/override (on-paint)
        (send (get-dc) draw-bitmap offscreen-bm 0 0))
      
      (define/private (pos->location i j)
        (values (add1 (* i scale)) (add1 (* (- height j 1) scale))))
  
      (define/private (location->pos x y)
        (values (min (max 0 (floor (/ (sub1 x) scale))) (sub1 width))
                (- height (min (max 0 (floor (/ (sub1 y) scale))) (sub1 height)) 1)))
  
      (define (pos->cell i j)
        (vector-ref (vector-ref board j) i))
                        
      (define water-rgn (make-object region% offscreen))

      (define/private (draw-board-pos dc i j non-water-rgn)
        (let-values ([(cell) (pos->cell i j)]
                     [(x y) (pos->location i j)])
          (send dc set-brush
                (case cell
                  [(water) 
                   (send water-rgn set-rectangle x y cell-paint-size cell-paint-size)
                   (send non-water-rgn subtract water-rgn)
                   water-brush]
                  [(wall) wall-brush]
                  [(plain) plain-brush]
                  [(base) base-brush]))
          (send dc draw-rectangle x y cell-paint-size cell-paint-size)))
  
      (define margin 2)
      
      (define/private (draw-package dc pack)
        (let*-values ([(x y) (pos->location (pack-x pack) (pack-y pack))]
                      [(icon) (car (pack-icon pack))]
                      [(dy) (if (pack-home? pack)
                                margin
                                (- cell-paint-size (send icon get-height) margin))]
                      [(dx) (if (pack-owner pack)
                                margin
                                (- cell-paint-size (send icon get-width) margin))])
          (send dc draw-bitmap icon (+ dx x) (+ y dy)
                'solid black 
                (cdr (pack-icon pack)))
          (let-values ([(dest-x dest-y) (if (pack-dest-x pack)
                                            (pos->location (pack-dest-x pack) (pack-dest-y pack))
                                                           (values #f #f))]
                       [(ddx) (/ (send icon get-width) 2)]
                       [(ddy) (/ (send icon get-height) 2)])
            (when dest-x
              (send dc set-pen (pack-pen pack))
              (send dc draw-line (+ x ddx dx) (+ y ddy dy) (+ dest-x dx ddx) (+ dest-y dy ddy))
              (send dc set-pen transparent-pen)))))
      
      (define/private (draw-robot dc robot)
        (let-values ([(x y) (pos->location (robot-x robot) (robot-y robot))])
          (when (robot-motion robot)
            (send dc set-brush motion-brush)
            (send dc set-pen black-pen)
            (send dc draw-polygon (robot-motion robot) x y)
            (send dc set-pen transparent-pen))
          (let ([icons (if (robot-dead? robot)
                           (robot-dead-icon robot)
                           (robot-icon robot))])
            (send dc draw-bitmap (car icons) (+ 1 x) y 
                  'solid black 
                  (cdr icons)))))
            
      (super-instantiate (frame))
      (stretchable-width #f)
      (stretchable-height #f)
      (min-client-width display-w)
      (min-client-height display-h))))
