#cs
(module if mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss"))

  ;; the interfaces and data definitons for Carcassonne 

  (define admin<%>
    (interface ()
      register ;; [-player] ;; player<%> -> (union false Follower)
      ;; sign up a player for game, assign Follower, if possible; #f otherwise
      
      register-observer ;; observer<%> -> Void 
      ;; sign up an observer for the game; always succeeds 
      
      run-game ;; -> String 
      ;; run the game and produce final message
      
      get-graph ;; -> drawable-graph<%>
      ;; get the drawable graph from the admin 
      
      ;; ASSUME: ({register | register-observer}+ . run-game)
      ))

  (define observer<%>
    (interface ()
      placed-tile ;; tile<%> -> Void 
      ;; some other player placed t during a turn 
      ;; ASSUME: it is legal to place the tile in the current graph 
      ;; ASSUME: placed-tile is not called while take-turn is active 
      
      placed-follower ;; tile<%> Follower Position -> Void 
      ;; some other player placed f on t at p 
      ;; ASSUME: it is legal to place f on t at ap in the current graph 
      ;; ASSUME: placed-follower is not called while take-turn is active 

      other-score-and-token ;; Follower Number Number -> Void 
      ;; player with follower f scored s and received n tokens back 
      ;; ASSUME: s >= 0, 7 >=n >=0
      ))
  
  (define admin-player<%>
    (interface ()
      take-turn ;; turn<%> -> Void
      ;; it's the player's turn 
      
      score-and-token ;; Number Number -> Void 
      ;; the player has scored a point and receives some of the tokens back
      ;; ASSUME: the first number is the score assigned with one of the recently 
      ;; completed regions; the second one is the number of followers of _this_ 
      ;; player that occupided this region 
      
      inform ;; String -> Void 
      ;; accept message 
      ))
  
  (define player<%>
    (interface (observer<%> admin-player<%>)))
  
  (define turn<%>
    (interface ()      
      get-index ;; -> Index
      ;; the index of the tile that you're allowed to place 
      
      potential-locations-for-tile ;; -> Listof[tile<%>]
      ;; given an index for a potential tile, compute a list of tiles
      ;; that could be placed into the graph
      
      place-tile ;; tile<%> -> Void
      ;; _this_ turn places the given tile into the graph 
      ;; ASSUME: the given tile is placable in _this_ graph,
      ;; in the sense of _potential-locations-for-tile_      
      
      potential-locations-for-followers	;; -> Listof[(list tile<%> Position)]
      ;; compute the potential tile locations and positions on these tiles
      ;; in _this_ graph where a follower could be placed 
      
      place-follower ;; tile<%> Position -> Void
      ;; this turn places the player's follower on t at p
      ;; ASSUME: the player has a follower to place 
      ;; ASSUME: the given coordinates and position are legal in the
      ;; sense of potential-locations-for-followers      
      
      ;; ASSUME: 
      ;;  (others* . place-tile . others* [ . place-follower ] . others*)
      ))
  
  (provide 
   turn<%>
   player<%>
   admin-player<%>
   admin<%>
   observer<%>)
  
  ;; ---------------------------------------------------------------------------
  
  (define player-graph<%>
    (interface () 
      
      potential-locations-for-tile ;; Index -> Listof[tile<%>]
      ;; given an index for a potential tile, compute a list of tiles
      ;; that could be placed into the graph
      ;; ASSUME: for each tile in the resulting list: 
      ;; it can be legally added to the graph 
      
      insert-tile ;; tile<%> -> graph<%>
      ;; add the given tile to _this_ graph 
      ;; ASSUME: the given tile is placable in _this_ graph,
      ;; in the sense of _potential-locations-for-tile_
      ;; ASSUME: the new graph contains the given tile 
      
      potential-locations-for-followers	;; -> Listof[(list tile<%> Position)]
      ;; compute the potential tile locations and positions on these tiles
      ;; in _this_ graph where a follower could be placed 
      ;; ASSUME: for each item (t,p) on the resulting list: 
      ;; placing any follower f on t at p is legal 
      
      place-follower ;; tile<%> Position Follower -> graph<%>
      ;; create graph from this Follower on Position at Tile 
      ;; ASSUME: the given coordinates and position are legal in the
      ;; sense of potential-locations-for-followers 
      ;; ASSUME: it is no longer possible to place a follower 
      ;; at this position on the given tile
      ))
  
  (define admin-graph<%>
    (interface ()
      complete-regions ;; -> Listof[completed<%>]
      ;; the list of regions that were completed by the last insert
      
      abbey-regions ;; -> Listof[(list Follower Number)]
      ;; compute who gets how many points for incomplete abbey regions
      ;; ASSUME: for each (f,n) on the resulting list: 
      ;; 0 < n < 9 [the max score you can get for an abbey]
      ))
  
  (define drawable-graph<%>
    (interface ()
      left ;; -> Number
      ;; the leftmost x index

      top ;; -> Number
      ;; the topmost y index

      list-of-tiles ;; -> Listof[tile<%>]
      ;; the list of tiles in the graph
      ))

  (define graph<%>
    (interface (admin-graph<%> player-graph<%> drawable-graph<%>) ))
  
  (define completed<%>
    (interface ()
      followers ;; -> Listof[(list Follower PositiveNumber)]
      ;; how many followers of each kind exist occupy _this_ completed region 
      ;; this can be used for both scoring and returning followers to players
      
      score ;; -> PositiveNumber
      ;; what is the value of _this_ completed region, independent of the 
      ;; followers on the tiles
      
      remove-followers ;; -> completed<%> 
      ;; effect: remove all followers from _this_ region 
      ;; return _this_ region 
      ;; ASSUME: (send this followers) is empty now 
      
      equal ;; completed<%> -> Boolean 
      ;; are _this_ and the given instance equal? 
      ))
  
  (define tile<%>
    (interface () 
      equal ;; tile<%> -> Boolean 
      ;; compare _this_ tile with the given tile 
      get-index ;; -> Index
      get-x ;; -> Number 
      get-y ;; -> Number 
      get-o ;; -> Number 
      ))
  
  (define (is? l)    (lambda (x) (member x l)))
  (define (string->x check?)
    (lambda (x)
      (let ([r (string->number x)])
        (unless (and r (check? r)) 
          (error 'string->direction "not a number string: ~e" x))
        r)))
  
  ;; Coordinates
  (define coordinate? number?)
  (define coordinate->string number->string)
  (define string->coordinate (string->x number?))
  
  ;; a Direction is one of:
  (define NORTH   0)
  (define EAST   90)
  (define SOUTH 180)
  (define WEST  270)
  
  (define directions (list NORTH EAST SOUTH WEST))
  (define direction? (is? directions))
  
  (define direction->string number->string)
  (define string->direction (string->x direction?))
  
  ;; functions for moving in these cardinal directions 
  ;; x :: the east-west coordinate 
  ;; y :: the north-south coordinate 
  (define north sub1)
  (define east  add1)
  (define south add1)
  (define west  sub1)
  
  ;; Orientation is one of: 
  (define orientations (list NORTH EAST SOUTH WEST))
  (define orientation? (is? orientations))
  
  (define orientation->string number->string)
  (define string->orientation (string->x number?))
  
  ;; a Position is one of:
  ;; -- Direction
  (define INNER -100)
  
  ;; interpretation: 
  ;; NORTH :: road or castle w/o interior connection 
  ;; SOUTH :: road or castle w/o interior connection 
  ;; EAST  :: road or castle w/o interior connection 
  ;; WEST  :: road or castle w/o interior connection 
  ;; INNER :: abbey or castle with interior connection
  
  (define position?  (is? (cons INNER directions)))
  (define (position=? x y) (and (position? x) (position? y) (= x y)))
  (define position->string number->string)
  (define string->position (string->x position?))
  
  ;; a Follower is one of: 
  (define RED   "red")
  (define WHITE "yellow")
  (define BLUE  "cyan")
  (define BLACK "black")
  (define GREEN "green")
  (define MAGENTA "magenta")
  
  (define followers (list RED WHITE BLUE BLACK GREEN MAGENTA))
  (define MAX-FOLLOWERS 7)
  
  (define follower? (is? followers))
  (define (all-followers) followers)
  
  (define (follower-string x) x)
  (define follower=? string=?)
  (define (string->follower s)
    (unless (and (string? s) (follower? s))
      (error 'string->follower "expected follower string, given ~e" s))
    s)
  ;; Follower -> bitmap%
  (define (follower-bm f)
    (make-object bitmap% (build-path "Followers" (format "f_~a.gif" f)) 'gif #f))
  
  ;; an Index is one of:
  (define indecies (cons "00" (map number->string (build-list 24 add1))))
  (define index? (is? indecies))
  (define index->string identity)

  (provide tile<%> 
           graph<%>
           admin-graph<%> 
           player-graph<%>
           
           completed<%>
           
           ;; Index 
           index? index->string 
           
           ;; Coordinate 
           coordinate? string->coordinate coordinate->string 
           
           ;; Orientation 
           orientation?
           orientation->string 
           string->orientation
           
           ;; Position
           position? INNER position=? position->string string->position
           
           ;; Direction
           direction? NORTH EAST SOUTH WEST string->direction 
           
           ;; Movements 
           north east south west 
           
           ;; Followers
           RED WHITE BLUE BLACK GREEN MAGENTA MAX-FOLLOWERS
           follower? follower-string follower-bm all-followers follower=? string->follower
           )
  
  ;; --- auxiliary things for directions --- 
  
  (provide 
   direction-opposite     ;; Direction -> Direction 
   dir-case               ;; Direction X X X X -> X 
   iterate-over-neighbors ;; Number Number (Number Number -> X) (Listof[X] -> Y) -> Y
   ;; apply f to each of the 8 neighbors 
   ;; and collect the results with combine 
   )
  
  ;; Directions
  
  (define (direction-opposite d) (dir-case d SOUTH WEST NORTH EAST))
  
  (define (dir-case d n e s w)
    (cond [(eq? d NORTH) n]
          [(eq? d EAST)  e]
          [(eq? d SOUTH) s]
          [(eq? d WEST)  w]
          [else (error 'direction "expected a direction; given ~e" d)]))
  
  (define (iterate-over-neighbors x y f combine)
    (combine (f x        (north y))
             (f (east x) (north y))
             (f (east x) y)
             (f (east x) (south y))
             (f x        (south y))
             (f (west x) (south y))
             (f (west x) y)
             (f (west x) (north y))))
  
  
  ;; --- quality of service specs --- 
  
  (define max-turn-time         (make-parameter 3))
  (define MAX-REGISTRATION-TIME 6)
  (define REG-WAIT              (make-parameter (* 60 10))) ;; 10 minutes
  (define MAX-RETURN-TIME       6)
  
  (provide max-turn-time MAX-REGISTRATION-TIME MAX-RETURN-TIME REG-WAIT)

  ;; --- 
  
  (define GAME-MACHINE (make-parameter "127.0.0.1"))
  (define PORT 7000)
  
  (provide PORT GAME-MACHINE)
  
  )
