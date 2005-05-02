#cs
(module graph mzscheme 

  ;; the evolving graph (landscape) and its completed regions 

  (require "if.scm"
           "tiles.scm"
           "aux.scm"
           "contract.scm"
           (file "Testing/testing.scm")
           (lib "list.ss")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "pretty.ss"))

  
  #| A Graph of Tiles 
     - The graph is a collection of tiles.
     - It represents a Carcassonne map.
  |# 
  
  (provide graph%
           completed=
           list-completed=)
  
  ;; providing for testing only 
  (provide (rename make-completed-ex make-completed))
  
  ;; Number Number Listof[tile<%>] -> (union #f tile<%>)
  ;; look for first tile with (x,y) as coordinats
  (define (lookup-tile x y tiles)
    (let loop ([tiles tiles])
      (cond
        [(null? tiles) #f]
        [else (let* ([fst (car tiles)])
                (if (and (= (send fst get-x) x) (= (send fst get-y) y))
                    fst
                    (loop (cdr tiles))))])))
  
  ;; --- Completed --- 
  (define completed% 
    (class object%
      (init ti f)
      (super-new)
      
      (field
       [fllwrs f]  ;; Listof[(list Followers tile<%> Position)]
       [tiles  (quicksort ti (lambda (p q) (send p less q)))])
      
      (define/public (followers)
        (define f (map car fllwrs))
        (define (add f h) ;; bump entry by 1 
          (map (lambda (x) (if (eq? (car x) f) (list f (+ (cadr x) 1)) x)) h))
        (set> (foldr add (map (lambda (x) (list x 0)) f) f) equal?))

      (define/public (remove-followers)
        (for-each (lambda (x) (send (cadr x) remove-follower (caddr x))) fllwrs)        
        (set! fllwrs '())
        this)

      ;; --- debugging aides --- 
      
      ;; -> Listof[tile<%>]
      (define/public (show) tiles)
      
      ;; completed<%> -> Boolean 
      ;; assumes that two different variants can't have the same type 
      (define/public (equal y) (list-tile= tiles (completed-tiles y)))))
  
  (define completed-tiles (class-field-accessor completed% tiles))
  
  (define cabbey%
    (class* completed% (completed<%>)
      (super-new)
      (inherit-field tiles fllwrs) ;; tiles contains exactly one tile 

      (define/public (score) 8)))
  
  (define road<%>
    (interface (completed<%>)
      
      merge ;; croad% croad% tile<%> Listof[Follower] -> (list road<%>)
      ;; merge two road structure

      add-followers ;; Listof[Follower] -> completed<%>
      ;; add the given followers to this structure 
      ))
  
  (define croad%
    (class* completed% (completed<%> road<%>)
      (super-new)
      (inherit-field tiles fllwrs)
      (define/public (score) (length tiles))
      
      (define/public (add-followers f)
        (set! fllwrs (append f fllwrs))
        this)
      
      (define completed-fllwrs (class-field-accessor completed% fllwrs))
      (define/public (merge that join jfollowers)
        (define a (remq join (completed-tiles this)))
        (define b (remq join (completed-tiles that)))
        (list
         (make-object croad%
           (append (list join) a b)
           (append jfollowers fllwrs (completed-fllwrs that)))))))
  
  (define ccastle% 
    (class* completed% (completed<%>)
      (init x y)
      (init-field [scr 0])
      (super-instantiate (x y))
      (define/public (score) scr)))

  (define (completed? x) (is-a? x completed<%>))

  ;; Type = 'abbey | 'castle | 'road
  ;; Type Listof[tile<%>] Listof[Follower] -> completed<%>
  (define (make-completed ty ti f) 
    (case ty
      ['abbey  (make-object cabbey% ti f)]
      ['castle (make-object ccastle% ti f)]
      ['road   (make-object croad% ti f)]
      [else (error 'contract "bad type for make-completed; given ~e" ty)]))
 
  (define make-completed-ex 
    (case-lambda 
      [(ty ti)   (make-completed ty ti '())]
      [(ty ti f) (make-completed ty ti f)]))
  
  (define (completed= x y) (send x equal y))
  
  ;; Listof[Completed] -> Boolean 
  (define list-completed= (list-object= completed=))
  
  ;; --- Graph --- 
  
  ;; represent a graph of tiles with 2-dim access and navigation 
  (define parent-inspector (current-inspector))
  (define pre-graph%
    (parameterize ([current-inspector (make-inspector)])
      (class* object% ()
        (super-new)        
        ;; --- internal state ----------------------------------------------------        
        (init-field
         [last-tile (START)]
         [the-tiles (list last-tile)]
         [history   '()]) ;; retains old stuff 

        (define/public (creation name)
          (pretty-print 
           `(define (,name)
              (let* ([graph0 (new graph%)]
                     ,@(map (lambda (x) `[graph0 ,x]) (reverse history)))
                graph0))))
        
        ;; -> Void
        (define/public (reset)
          (set! last-tile (START))
          (set! the-tiles (list last-tile)))
        
        ;; tile<%> Direction -> (union false tile<%>)
        ;; exiting the current tile in exit-dir, what's the next spot 
        (define/public (going-to current exit-dir)
          (let ([x (send current get-x)][y (send current get-y)])
            (cond
              [(eq? NORTH exit-dir) (lookup x        (north y))]
              [(eq? EAST  exit-dir) (lookup (east x) y)]
              [(eq? SOUTH exit-dir) (lookup x        (south y))]
              [(eq? WEST  exit-dir) (lookup (west x) y)]
              [else (error 'going-to "bad direction: ~s~n" exit-dir)])))
        
        ;; Number Number -> (union tile<%> false)
        (define/public (lookup x y) (lookup-tile x y the-tiles))
        
        ;; Number Number -> Boolean 
        ;; is a tile at these coordinates? 
        (define/public (exists? x y) (is-a? (lookup x y) tile<%>))
        
        ;; tile<%> -> Void
        ;; effect: add tile to the-tiles 
        (define/public (add-tile! p)
          (set! last-tile p)
          (set! the-tiles (cons last-tile the-tiles))
          (set! history (cons (action p) history)))
        
        ;; tile<%> -> graph%
        (define/public (add-tile p)
          (parameterize ([current-inspector parent-inspector])
            (let-values ([(this-class% s) (object-info this)])
              (new this-class%
                   [the-tiles (cons p the-tiles)]
                   [last-tile p]
                   [history   (cons (action p) history)]))))
        
        ;; tile<%> tile<%> S-exp -> graph%
        ;; replace p with q in the-tiles
        (define/public (update p q . a)
          (parameterize ([current-inspector parent-inspector])
            (let-values ([(this-class% s) (object-info this)])
              (new this-class%
                   [the-tiles (cons q (remove p the-tiles (lambda (p x) (send p equal x))))]
                   [last-tile last-tile]
                   [history   (if (pair? a) (cons (car a) history) history)]))))
        
        (define/private (action p)
          (define i (send p get-index))
          (define x (send p get-x))
          (define y (send p get-y))
          (define o (send p get-o))
          `(send graph0 insert-tile (make-tile ,i ,x ,y ,o)))
        
        ;; --- public : drawable -------------------------------------------------
        
        (define/public (left) (extract (lambda (t) (send t get-x))))
        
        (define/public (top) (extract (lambda (t) (send t get-y))))
        
        ;; (tile<%> -> Number) -> Number
        (define/private (extract m) (apply min (map m the-tiles)))
        
        (define/public (list-of-tiles) (set> (reverse the-tiles) tile=)))))
  
  (define graph%
    (parameterize ([current-inspector (make-inspector)])
      (class* pre-graph%  (admin-graph<%> player-graph<%> #;drawable-graph<%>)
        (super-new)
        (inherit add-tile! add-tile exists? lookup reset going-to update)
        (inherit-field history last-tile the-tiles)
        
        ;; --- public : administrator --------------------------------------------
        
        ;; --- end of game scoring --- 
        (define/public (abbey-regions . remove)
          (define rem (pair? remove)) 
          (assert
           (lambda (result) (andmap (lambda (x) (<= 0 (cadr x) 8)) result))
           (map (lambda (t)
                  (define f (car (send t follower@ INNER)))
                  (when rem (send t remove-follower INNER))
                  (list f
                        (iterate-over-neighbors
                         (send t get-x)
                         (send t get-y)
                         (lambda (x y) (if (boolean? (lookup x y)) 0 1))
                         +)))
                (filter (lambda (t)
                          (and (send t abbey?) (pair? (send t follower@ INNER))))
                        the-tiles))
           "illegal number of followers returned: ~s"))

        ;; --- placing a follower 
        (define/public (place-follower t p f)          
          (define ts (potential-locations-for-followers))
          (unless (is-a? t tile<%>)
            (error 'place-follower "expected tile, given ~e" t))
          (unless (member* t ts (lambda (x y) (and (tile= (car x) t) (= (cadr x) p))))
            (contract/violation
             (format "expected tile with free position for follower, given: (~s,~s) @ ~s"
                     (send t get-x) (send t get-y) p)))
          (let ((tquote `(send graph0 lookup ,(send t get-x) ,(send t get-y))))
            (update t (send t place-follower p f)
                    `(send graph0 place-follower ,tquote ,p ,f))))
        
        ;; --- where to place a follower?
        ;; the list of coordinates and positions where a follower could be placed
        
        (define/public (potential-locations-for-followers)
          (apply append (map potential-locations-for-followers-at the-tiles)))
        
        
        ;; tile<%> -> Listof[(list tile<%> Position)]
        ;; which positions on t can a follower occuy still?
        (define (potential-locations-for-followers-at t)
          (define x (send t get-x))
          (define y (send t get-y))
          (define ps (send t follower-places))
          (map (lambda (f) (list (lookup x y) f)) 
               (filter (lambda (p) [(incomplete&unoccupied-region? t) p]) ps)))
        
        ;; tile<%> -> (Position -> Boolean)
        ;; ensure that the region is incomplete and that no followers are occupying it
        (define/private (incomplete&unoccupied-region? t)
          (lambda (pos)
            (unless (null? (send t follower@ pos))
              (error 'contract
                     "incomplete&unoccupied assumes ~s is unoccupied at ~s" 
                     t pos))
            (case (send t region@ pos)
              ['abbey (null? (trace-abbey t))] ;; pos must be INNER
              ['castle (null? (trace-castles t (send t castles)))]
              ['road ;; road => pos is not INNER, so trace the road; 
                (let-values ([(comp-in f-in) (comp-and-f t pos)])
                  (cond
                    [(send t end-of-road?) (and (null? f-in) (null? comp-in))]
                    [else
                     (let-values ([(comp-out f-out)
                                   (comp-and-f t (exit-dir t pos))])
                       (and (null? f-in) (null? f-out)
                            (or (null? comp-in) (null? comp-out))
                            (no-followers comp-in) (no-followers comp-out)))]))]
              [else (error 'contract "expected regionl given ~e" 
                           (send t region@ pos))])))
        
        ;; Listof[Completed] -> Boolean 
        (define/private (no-followers c*)
          (andmap (lambda (c) (null? (send c followers))) c*))
        
        ;; tile<%> Direction -> Listof[Completed] Listof[Followers]
        (define/private (comp-and-f t pos)
          (let* ([tt (trace-road t pos)])
            (values (filter completed? tt) (filter (compose not completed?) tt))))
        
        ;; --- scoring : did the last tile added complete any structures? 
        ;; the list of regions that were completed by the last insert
        
        (define/public (complete-regions)
          (define a (complete-abbeys last-tile))
          (define r (complete-roads last-tile))
          (define c (complete-castles last-tile))
          (cr-contract a "abbey")
          (cr-contract r "road")
          (cr-contract c "castle")
          (set> (append a r c) completed=))
        
        ;; Listof[X] String -p> Void
        (define (cr-contract a t)
          (unless (andmap completed? a)
            (error 'contract "not a list of completed regions (~a)~s~n" a t)))
        
        ;; tile<%> -> Listof[completed<%>]
        ;; did last-tile complete an abbey or did it complete an abbey neighbor
        (define/private (complete-abbeys last-tile)
          (define (tr x) (filter completed? (trace-abbey x)))
          (append
           (tr last-tile)
           (iterate-over-neighbors (send last-tile get-x) (send last-tile get-y)
                                   (lambda (x y) (tr (lookup x y)))
                                   (lambda x (apply append x)))))

        ;; -> Listof[completed<%>]
        ;; may contain repeated completed castles! 
        (define/private (complete-castles last-tile)
          (define c (send last-tile castles))
          (define (tr last-tile castle-exits)
            (filter completed? (trace-castles last-tile castle-exits)))
           (if (send last-tile castle?)
               ;; all "castle exits" from this tile belong to the same castle:
               (tr last-tile c)
               ; (car c) (if (null? (cdr c)) '() (list (cons last-tile (cdr c))))
               ;; if there is more than one "castle exit", 
               ;; it may finish off several castles:
               (apply append (map (lambda (s) (tr last-tile (list s))) c))))
        
        ;; tile<%> -> Listof[completed<%>]
        ;; find all possible roads that were completed by the last insertion
        (define (complete-roads last-tile)
          (define r (send last-tile roads))
          (define (tr x y) (filter completed? (trace-road x y)))
          (cond
            [(send last-tile end-of-road?)
             ;; last-tile ends one or more of the roads to which it is connected 
             (apply append
                    (map (lambda (dir)
                           (let ([res (tr last-tile dir)]
                                 [fll (pin-follower last-tile dir)])
                             (if (pair? res) 
                                 (list (send (car res) add-followers fll))
                                 '())))
                         r))]
            ;; last-tile connects two parts of a roads with endings            
            [(= (length r) 2) 
             (let* ([one-dir (car r)]
                    [other-dir (cadr r)]
                    [a (tr last-tile one-dir)] ;; one direction 
                    [b (tr last-tile other-dir)] ;; other direction
                    [f (pin-follower last-tile one-dir)]
                    [g (pin-follower last-tile other-dir)]) 
               (if (and (pair? a) (pair? b))
                   (send (car a) merge (car b) last-tile (append f g))
                   '()))]
            ;; no roads, no completed roads
            [(null? r) '()]
            ;; last-tile either satisfies end-of-road? 
            ;; or has exactly two or exactly no road exists
            [else (error 'contract "complete-roads: can't happen ~s" r)]))

        ;; --- tracing regions --- 
        
        ;; (union false tile<%>) -> (union null (list Completed))
        ;; if is this tile an abbey and do the eight neigbors exist, a list 
        (define/private (trace-abbey i)
          [define e? (lambda (i j) (exists? i j))]
          (if (and i (send i abbey?) 
                   (iterate-over-neighbors (send i get-x) (send i get-y) e? and*))
              (list (make-completed 'abbey (list i) (pin-follower i INNER)))
              '()))        

        ;; tile<%> (cons Direction Listof[Direction])
        ;;   -> (union Listof[Follower] (list Completed))
        ;; compute the completed castle region starting at current [tile]
        ;;  with given [castle] exits from this tile 
        ;; if incomplete region, return followers (if any) in this region 
        (define/private (trace-castles current exits)
          [define (score next) (if (send next flag?) 4 2)]
          [define (scr++ next) (set! scr (+ (score next) scr))]
          [define scr (score current)]
          [define followers (apply append (map (lambda (x) (send current follower@ x)) exits))]
          [define (extend-followers next pos) 
            (set! followers (append followers (pin-follower next pos)))]
          [define (trace-this next from-dir todo tiles-in-region-so-far)
            ;; todo: Listof[(cons tile<%> (cons Direction Listof[Direction]))
            (define in-dir (direction-opposite from-dir))
            (cond
              [(boolean? next) 
               ;; the castle exit went out of scope; it's incomplete
               (begin (unless (null? todo) ;; collect rest of followers if any
                        (more-to-trace todo tiles-in-region-so-far))
                      followers)]
              [(member next tiles-in-region-so-far)
               (unless (send next castle?) ;; disconnected castle pieces 
                 ;; because you entered this tile from a new direction:
                 (extend-followers next in-dir)
                 (scr++ next))
               (more-to-trace todo tiles-in-region-so-far)]
              [else 
               (scr++ next)
               (let* ([c (remq in-dir (send next castles))]
                      [belong (cons next tiles-in-region-so-far)])
                 (if (send next castle?) ;; implies (not (null? c))
                     ;; several sides of the tile are castle and they are connected
                     ;; make sure the castle is complete in all directions 
                     (begin
                       (extend-followers next INNER)
                       (trace-from next c todo belong))
                     ;; only one entrance point into the tile from a castle
                     ;; nothing else to do for next
                     (begin
                       (extend-followers next in-dir)
                       (more-to-trace todo belong))))])]
          [define (more-to-trace todo belong) 
            (if (null? todo)
                (list (make-object ccastle% belong followers scr))
                (trace-from (caar todo) (cdar todo) (cdr todo) belong))]
          [define (trace-from current exits t v) 
            (define side (car exits))
            (define todo (if (null? (cdr exits)) t (cons (cons current (cdr exits)) t)))
            (trace-this (going-to current side) side todo v)]
          ;; -- go: -- 
          (trace-from current exits '() (list current)))
        
        ;; tile<%> Direction -> (union Listof[Follower] (list Completed))
        ;; trace a road leaving from the lt leaving thru dir
        ;; do not include token for lt!
        ;; -> Completed, if the road has two ends 
        ;; -> Listof[Follower] otherwise (for placing followers)
        (define (trace-road lt dir)
          (let L ([crt (going-to lt dir)][exit-lt-dir dir][seen (list lt)][f '()])
            (define entry-dir (direction-opposite exit-lt-dir))
            (cond
              [(boolean? crt) f] 
              ;; infinite loops shouldn't get points, so occupy them randomly 
              ;; WHY? OH WHY? DID I DO THIS? 
              [(member crt seen) `(,RED)] 
              [(send crt end-of-road?) 
               (let ([g (pin-follower crt entry-dir)])
                 (list (make-completed 'road (cons crt seen) (append g f))))]
              [else (let* ([exit (exit-dir crt entry-dir)]
                           [next (going-to crt exit)]
                           [g (pin-follower crt entry-dir)]
                           [h (pin-follower crt exit)])
                      (L next exit (cons crt seen) (append f g h)))])))
        
        ;; tile<%> Direction -> Direction 
        ;; contract: roads exist, but end-of-road? is false 
        ;; .: there are two roads -- take the one not taken in
        ;; the current tile was entered in entry-dir on a road
        ;; where do we exit and to what tile do we go        
        (define (exit-dir current entry-dir)
          (let* ([r (send current roads)]
                 [one (car r)])
            (if (eq? one entry-dir) (cadr r) one)))
                
        ;; tile<%> Position -> Listof[(list Follower tile<%> Position)]
        (define (pin-follower tile pos)
          (map (lambda (f) (list f tile pos)) (send tile follower@ pos)))
        
        ;; --- potential-locations-for-tile
        
        (field [hash-potential-locations '()])
        ;; Index -> (union false Listof[tile<%>])
        (define/private (hash-look i)
          (let ([r (assoc i hash-potential-locations)])
            (if r (cdr r) r)))
        
        ;; Index -> Listof[tile<%>]
        ;; produce tiles! -- you need to know which (x,y) and which orientation
        (define/public (potential-locations-for-tile i)
          (define r (hash-look i))
          (if (boolean? r)
              (let ([r (foldr (lambda (f r) (append (potential-loc-at f i) r)) '() the-tiles)])
                (set! hash-potential-locations 
                      (cons (cons i r) hash-potential-locations ))
                r)
              r))
        
        ;; tile<%> Index -> Listof[tile<%>]
        ;; which of the four sides of tile can accommodate a tile with index i
        ;; and if so at what orientation 
        (define/private (potential-loc-at tile i)
          (send tile map-neighbors (fits i) append))
        
        ;; fits : Index -> (Direction Number Number -> Listof[tile%])
        ;; compute all possible orientations at which i fits into the graph 
        ;; at (x,y) and produce the corresponding list of tiles 
        (define/private (fits i)
          (lambda (dir x y)
            (if (exists? x y)
                '()
                (apply append (map (fits-single i x y) '(0 90 180 270))))))
        
        ;; fit-single : Index Number Number -> (Orientation -> Listof[tile%])
        ;; if tile (i,x,y,o) fits, produce a singleton list; otherwise '()
        (define/private (fits-single i x y)
          (lambda (o) 
            (define nt (make-tile i x y o))
            (if (send nt map-neighbors (tile-match nt) and*) (list nt) '())))
        
        ;; tile<%> -> (Direction Number Number -> Boolean)
        ;; does tile match with the tile at (x,y) [if any] facing off at dir
        (define/private (tile-match nt)
          (lambda (dir nx ny)
            (define p (lookup nx ny))
            (or (boolean? p) (send nt match dir p))))
        
        ;; --- insert-tile 
        
        ;; tile<%> FormatString -> Void
        ;; compose and signal a contract violation 
        (define/private (signal p  msg)
          (define (s p)
            (list
             (send p get-x) (send p get-y) (send p get-index) (send p get-o)))
          (define loc (potential-locations-for-tile (send p get-index)))
          (contract/violation (format msg (s p) (map s loc))))
        
        ;; tile<%> -> Boolean 
        (define (insert-tile-contract p)
          (member* p (potential-locations-for-tile (send p get-index)) tile=))

        (define/public (insert-tile! p) 
          (if (insert-tile-contract p)
              (begin (add-tile! p)
                     (set! hash-potential-locations '()))
              (signal
               p "insert-tile! expected a placable tile; given ~s, placable ~e")))
        
        (define/public (insert-tile p)
          (if (insert-tile-contract p)
              (add-tile p)
              (signal
               p "insert-tile expected a placable tile; given ~e, placable ~e")))

        #| INTERNAL TESTS:       |#
        (define/public (test)          
          (define graph0 this)
          (define graph1 (send graph0 insert-tile (make-tile "2" +1 0 90)))
          
          (printf "testing graph ... ~n")

          (test== ((tile-match (make-tile "2" 0 -1 0)) WEST  -1 -1) #t "pm west")
          (test== ((tile-match (make-tile "2" 0 -1 0)) EAST  +1 -1) #t "pm east")
          (test== ((tile-match (make-tile "2" 0 -1 0)) NORTH  0 -2) #t "pm nortn")
          (test== ((tile-match (make-tile "2" 0 -1 0)) SOUTH  0  0) #f "pm south")
          
          (test== ((tile-match (make-tile "2" 0 -1 90)) WEST  -1 -1) #t "pm west 90")
          (test== ((tile-match (make-tile "2" 0 -1 90)) EAST  +1 -1) #t "pm east 90")
          (test== ((tile-match (make-tile "2" 0 -1 90)) NORTH  0 -2) #t "pm north 90")
          (test== ((tile-match (make-tile "2" 0 -1 90)) SOUTH  0  0) #f "pm south 90")
          
          (test== ((tile-match (make-tile "1" 0 -1   0)) NORTH 0 0) #t "match 1(0) north")
          (test== ((tile-match (make-tile "1" 0 -1  90)) NORTH 0 0) #t "match 1(90) north")
          (test== ((tile-match (make-tile "1" 0 -1 180)) NORTH 0 0) #t "match 1(180) north")
          
          (test== ((tile-match (make-tile "1" 0 +1   0)) SOUTH 0 0) #f "match 1(0) south")
          (test== ((tile-match (make-tile "1" 0 +1  90)) SOUTH 0 0) #f "match 1(90) south")
          
          (test== ((tile-match (make-tile "2" +1 0 180)) SOUTH 0 0) #f "match 2(180) south")
          (test== ((tile-match (make-tile "2" +1 0  90)) SOUTH 0 0) #f "match 2(90) south")
          
          
          (test-eq (begin
                     (send this insert-tile! (make-tile "1" 0 1 90))
                     (send this insert-tile! (make-tile "2" -1 0 270))
                     (send this insert-tile! (make-tile "19" 1 0 0))
                     (going-to (make-tile "19" 1 0 0) WEST))
                   (make-tile "00" 0 0 0)
                   tile=
                   "going-to 1")
          (reset)
          
          (test-eq (begin
                     (send graph0 insert-tile! (make-tile "2" -1 0 270))
                     (send graph0 insert-tile! (make-tile "1" 0 1 0))
                     (send graph0 insert-tile! (make-tile "14" 0 2 90))
                     (send graph0 insert-tile! (make-tile "20" 1 0 0))
                     (send graph0 insert-tile! (make-tile "22" 1 1 0))
                     (send graph0 insert-tile! (make-tile "20" 1 2 270))
                     (send graph0 insert-tile! (make-tile "7" 1 3 180))
                     (send graph0 insert-tile! (make-tile "17" 2 1 180))
                     (send graph0 insert-tile! (make-tile "19" 2 0 0))
                     (going-to last-tile SOUTH))
                   (make-tile "17" 2 1 180)
                   tile=
                   "going-to 3")
          (reset)
          
          (test-p (begin
                    (send graph0 insert-tile! (make-tile "2" -1 0 270))
                    (send graph0 insert-tile! (make-tile "1" 0 1 0))
                    (send graph0 insert-tile! (make-tile "14" 0 2 90))
                    (send graph0 insert-tile! (make-tile "20" 1 0 0))
                    (send graph0 insert-tile! (make-tile "22" 1 1 0))
                    (send graph0 insert-tile! (make-tile "20" 1 2 270))
                    (send graph0 insert-tile! (make-tile "7" 1 3 180))
                    (send graph0 insert-tile! (make-tile "17" 2 1 180))
                    (send graph0 insert-tile! (make-tile "19" 2 0 0))
                    (trace-road (make-tile "19" 2 0 0) SOUTH))
                  (lambda (p)
                    (completed=
                     (car p)
                     (make-completed 'road 
                                     (list (make-tile "19" 2 0 0)
                                           (make-tile "17" 2 1 180)
                                           (make-tile "22" 1 1 0)
                                           (make-tile "20" 1 2 270)
                                           (make-tile  "7" 1 3 180))
                                     '())))
                  "trace-road")
          (reset)
          
          (printf "...done~n")))))
  
  
  
  (define graph-tiles (class-field-accessor pre-graph% the-tiles))
  
  )
