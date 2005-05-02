#cs
(module tiles mzscheme 

  ;; turn indexes into tiles and a whole bunch of methods 

  (require "if.scm"
           "tile-info.scm"
           "aux.scm"
           (file "Testing/testing.scm")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss"))
  
  
  (provide 
   graph-tile<%> ;; Interface 
   ;; the interface for the graph
   
   make-tile     ;; Index Number Number Orientation -> tile<%>   
   ;; factory method for a tile 
   
   tile? ;; Any -> Boolean 
   ;; is it a tile?

   tile= ;; tile<%> tile<%> -> Boolean 

   list-tile= ;; Listof[tile<%>] Listof[tile<%>] -> Boolean 
   
   START         ;; -> tile<%>
   ;; the first tile (normal orientation)
   )
  
   (define graph-tile<%>
    (interface (tile<%>)
      map-neighbors ;; (Direction Number Number -> X) (X X X X -> Y) -> Y
      ;; apply f to all cardinal directions and coordinates
      ;; collect the result with c
      
      match ;; Direction tile<%> -> Boolean
      ;; does this tile match in dir tile p? 
      
      abbey? ;; -> Boolean 
      ;; is this tile an abbey? 
      
      end-of-road? ;; -> Boolean 
      ;; does this tile end a road? 
      
      roads ;; -> Listof[Direction]
      ;; in which directions do the roads leave from here 
      
      castles      ;; -> Listof[Direction]
      ;; in which directions do castle pieces point 
      
      castle?      ;; -> Boolean 
      ;; is the content of index a castle?
      
      flag? ;; Boolean 
      ;; is the tile a flagged castle? 
      
      snip ;; -> snip% 
      ;; produce a snip 
      
      region@ ;; Position -> Content 
      
      follower@ ;; Position -> (union (list Follower) null)
      ;; retrieve the follower at p, if any 
      
      all-followers ;; -> (Listof Follower)
      ;; the list of followers on this tile 
            
      follower-places ;; -> Listof[Position]
      ;; where can you still place followers on this tile
 
      place-follower ;; Position Follower -> graph-tile<%>
      ;; effect: occupy the specified position on this tile with the follower
      ;; contract: the position is available 
      
      remove-follower ;; Position -> Void
      ;; effect: remove the follower from the specified position on this tile
      ;; contract: the position is occupied 
      
      to-string ;; -> String 
      ))
  
  ;; representing the tiles of the map 
  (define tile%
    (class* object% (tile<%>)
      (super-new)
      ;; --- internal description --- 
      (init-field index x y o) ;; CONTRACT: (string=? index "00") => (= o 0)
      (define/public (get-index) index)
      (define/public (get-x) x)
      (define/public (get-y) y)
      (define/public (get-o) o)
      (field [descriptor (lookup index o)])
      
      ;; --- comparisons ---
      (define/public (equal that)
        (and (string=? index (tile-index that))
             (= x (tile-x that))
             (= y (tile-y that))
             (= o (tile-orientation that))))

      (define/public (less that)
        (or 
         (< (string->number index) (string->number (tile-index that)))
         (and 
          (= (string->number index) (string->number (tile-index that)))
          (<= (get-x) (send that get-x)))))
      
      ;; SIDES -> Listof[Direction]
      ;; which sides are str
      (define/private (desc-sides str)
        ;; Direction -> (union (list Direction) null)
        (define (f dir)
          (define s (dir-case dir desc-north desc-east desc-south desc-west))
          (define x (s descriptor))
          (if (string=? str x) (list dir) '()))
        (append (f NORTH) (f EAST) (f SOUTH) (f WEST)))
      
      ;; --- neighbors --- 
      (define/public (map-neighbors f c)
        (c (f NORTH x (north y)) (f EAST (east x) y) (f SOUTH x (south y)) (f WEST (west x) y)))
      
      (define/public (match dir1 p)
        (string=? (desc-ref descriptor dir1) 
                  (desc-ref (tile-descriptor p) (direction-opposite dir1))))
      
      ;; --- abbey --- 
      (define/public (abbey?) (string=? (desc-content descriptor) "abbey"))
      
      ;; --- roads --- 
      (define/public (roads) (desc-sides "road"))
      
      (define/public (end-of-road?)
        (define c (desc-content descriptor))
        (define r (desc-sides "road"))
        (or (string=? c "end-of-road") 
            (= (length r) 1)
            (and (> (length r) 1) (not (castle?)) (not (string=? c "none")))))
      
      ;; --- castles --- 
      (define/public (castles) (desc-sides "castle"))
      
      (define/public (castle?) 
        (define c (desc-content descriptor))
        (or (string=? c "flag") (string=? c "castle")))
      
      (define/public (flag?) 
        (string=? (desc-content descriptor) "flag"))
      
      ;; --- snips and strings --- 
      (define/public (to-string) (format "~s" (list index (list x y) o)))
      
      (define/public (snip) 
        (define snp (index-snip index o))
        (define add (snip+follower snp))
        (for-each (lambda (p) (when (pair? p) (apply add p))) followers)
        snp)

      ;; snip% -> (Position Follower -> snip%)
      (define (snip+follower snip)
        (lambda (p f)
          (define bm (send snip get-bitmap)) 
          (define __ (send snip set-bitmap (make-object bitmap% 1 1 #f)))
          (define dc (new bitmap-dc% (bitmap bm))) 
          (define wi (send bm get-width))
          (define hi (send bm get-height))
          (define fbm (follower-bm f))
          (define fwi (send fbm get-width))
          (define fhi (send fbm get-height))
          (define-values (x y)
            (if (eq? p INNER)
                (values (/ (- wi fwi) 2) (/ (- hi fhi) 2))
                (apply values
                       (dir-case p
                                 (list (/ (- wi fwi) 2) (- 0 (/ fhi 2)))
                                 (list (- wi (/ fwi 2)) (/ (- hi fhi) 2))
                                 (list (/ (- wi fwi) 2) (- hi (/ fhi 2)))
                                 (list (- (/ fwi 2))    (/ (- hi fhi) 2))))))
          (send dc draw-bitmap fbm x y)
          (send dc set-bitmap #f)
          (send snip set-bitmap bm)
          snip))
      
      ;; --- accesssing objects of the same class --- 
      (define tile-x (class-field-accessor tile% x))
      (define tile-y (class-field-accessor tile% y))
      (define tile-index (class-field-accessor tile% index))
      (define tile-orientation (class-field-accessor tile% o))
      (define tile-descriptor (class-field-accessor tile% descriptor))
      
      ;; --- followers ---
      ;; Desc -> Listof[Position]
      (define/public (find-followers)
        (define a (abbey?))
        (define r (roads))
        (define c (castles))
        (define c? (castle?))
        (define e? (end-of-road?))
        (define con (desc-content descriptor))
        (cond
          [(and a (null? r)) (list INNER)]
          [(and a (pair? r) e?) (list INNER (car r))]
          [else (append (if (and (pair? c) c?)
                            (list INNER)
                            c)
                        (if (and (pair? r) (not e?))
                            (list (car r))
                            r))]))
      
      ;; Listof[(union Position (list Position Follower))]
      ;; where are the followers placed and where can others be placed
      (init-field [followers (find-followers)])

      (define/public (follower-places) (filter (compose not pair?) followers))
      
      (define/public (all-followers) (map cadr (filter pair? followers)))
      
      (define/public (follower@ p)
        ;; the resulting list contains one or no follower
        (map cadr (filter (lambda (x) (and (pair? x) (eq? (car x) p))) followers)))
      
      (define/public (region@ pos)
        (define r (roads))
        (cond
          [(eq? pos INNER) (if (abbey?) 'abbey 'castle)]
          [else (if (memq pos r) 'road 'castle)]))
      
      (define/public (place-follower p f)
        (define (add-follower q) (if (eq? p q) (list p f) q))
        (unless (memq p (follower-places))
          (error 'contract
                 "place-follower: given position ~e; free position expected: ~e"
                 p (follower-places)))
        (new tile% (index index) (x x) (y y) (o o) (followers (map add-follower followers))))
      
      (define/public (place-follower! p f)
        (define (add-follower q) (if (eq? p q) (list p f) q))
        (unless (memq p (follower-places))
          (error 'contract
                 "place-follower: given position ~e; free position expected: ~e"
                 p (follower-places)))
        (set! followers (map add-follower followers)))
      
      (define/public (remove-follower p)
        (define (rem-follower q) (if (and (pair? q) (eq? (car q) p)) p q))
        (when (memq p (follower-places))
          (error 'contract
                 "rem-follower: given position ~e; occupied position expected: ~e"
                 p (follower-places)))
        (set! followers (map rem-follower followers)))

      ))

  (define (make-tile a b c d) (new tile% [index a][x b][y c][o d]))
  
  (define (tile? x) (is-a? x tile%))

  (define (tile= p q) (send p equal q))
  
  (define list-tile= (list-object= tile=))

  (define (START) (make-tile "00" 0 0 0)) ;; needs aux functions

  )
