#cs
(module player mzscheme 
  
  ;; the basic player 
  
  (require "if.scm"
           "aux.scm"
           (file "Testing/testing.scm")
           (lib "class.ss"))
  
  (provide player%
           player-bad0%
           player-bad1%           
           player-bad2%
           player-bad3%           
           player-bad4%
           player-bad5%           
           player-bad6%
           player-with-view ;; player<%>-class -> player<%>-class 
           )
  
  (define (player-with-view player%)
    (class* player% (player<%>)
      (super-new)
      (inherit-field name)      
      (field [o (open-output-file (build-path "Tmp" (format "~a.txt" name))
		  'replace)])
      (define/override (inform s) (fprintf o "~a was told: ~a~n" name s))
      (define/override (placed-tile t) 
        (inform (format "placed-tile ~s~n" (send t to-string))))
      
      (define/override (placed-follower t f p) 
        (inform (format "placed-follower ~a on ~s @ ~s~n" f (send t to-string) p)))
      
      (define/override (other-score-and-token f s n) 
        (inform 
         (format "~a player scored ~s points and got ~s followers back~n" f s n)))))
  
  ;; a primitive random-strategy player 
  (define player%
    (class* object% (player<%>)
      (super-new)
      
      (init-field name)
      (define/public (get-name) name)
      
      (field [followers MAX-FOLLOWERS]
             [score     0])
      
      (define/public (take-turn t) 
        (send t place-tile (choose (send t potential-locations-for-tile)))
        (when (> followers 0)
          (let ([l (send t potential-locations-for-followers)])
            (when (and (pair? l) (odd? (random 10)))
              (let* ([choic (choose l)])
                (set! followers (- followers 1))
                (send t place-follower (car choic) (cadr choic)))))))
      
      (define/public (score-and-token s f) 
        (set! followers (+ f followers))
        (set! score     (+ s score)))
      
      (define/public (placed-tile t) 
        (inform (format "placed-tile ~s~n" (send t to-string))))
      
      (define/public (placed-follower t f p) 
        (inform
         (format "placed-follower ~a on ~s @ ~s~n" f (send t to-string) p)))
      
      (define/public (other-score-and-token f s n) 
        (inform 
         (format "~a player scored ~s points and got ~s followers~n" f s n)))
      
      (define/public (inform s) (void))))
  
  ;; this player raises an exception 
  (define player-bad0% 
    (class* player% (player<%>)
      (super-new)
      (define/override (take-turn t)
        (/ 1 0))))
  
  ;; this player won't place a tile after requesting all feasible locations 
  (define player-bad1% 
    (class* player% (player<%>)
      (super-new)
      (define/override (take-turn t)
        (define placable (send t potential-locations-for-tile))
        (void))))
  
  ;; this player will try to place two tiles
  (define player-bad2% 
    (class* player% (player<%>)
      (super-new)
      (define/override (take-turn t)
        (define placable (send t potential-locations-for-tile))
        (send t place-tile (car placable))
        (when (pair? (cdr placable)) (send t place-tile (cadr placable))))))
  
  ;; this player will try to place a follower before it places a tile
  (define player-bad3%
    (class* player% (player<%>)
      (super-new)
      (define/override (take-turn t)
        (define tile-places (send t potential-locations-for-tile))
        (define follrs-tile (send t potential-locations-for-followers))
        (when (pair? follrs-tile)
          (let ([fst (car follrs-tile)])
            (send t place-follower (car fst) (cadr fst)))))))
  
  ;; this player will try to place two follower after placing a tile 
  (define player-bad4%
    (class* player% (player<%>)
      (super-new)
      (inherit-field followers)
      
      (define/override (take-turn t)
        (define tile-places (send t potential-locations-for-tile))
        (define follrs-tile (send t potential-locations-for-followers))
        (send t place-tile (car tile-places))
        ;; place two followers:
        (when (pair? follrs-tile)
          (place-follower t follrs-tile)
          (place-follower t (cdr follrs-tile))))
      
      ;; turn<%> Listof[(list tile<%> Position)] -> Void      
      (define/private (place-follower t follrs-tile)
        (when (and (pair? follrs-tile) (> followers 0))
          (let* ([fst (car follrs-tile)])
            (send t place-follower (car fst) (cadr fst))
            (set! followers (- followers 1)))))))
  
  ;; this player will crash on second inform 
  (define player-bad5%
    (class* player% (player<%>)
      (super-new)
      (define/override (inform t)
        (cond
          [first-time (set! first-time #f) (printf "BAD:~n~a~n" t)]
          [else (/ 1 0)]))
      (field [first-time #t])))
  
  ;; this player will try to place two followers after placing a tile 
  (define player-bad6%
    (class* player% (player<%>)
      (super-new)
      (define/override (placed-follower t p f) (/ 1 0))))
  
  ;; --- tests --- 
  
  (define test-turn%
    (class* object% ()
      (super-new)
      (define/public (get-index) "00") 
      
      (define te 'TEST)
      (define/public (potential-locations-for-tile) `(,te)) 
      
      (define/public (place-tile t) (test== t te))
      
      (define et '(A B))
      (define/public (potential-locations-for-followers) `(,et))
      
      (define/public (place-follower t p) (test== et `(,t ,p)))))
  
  (provide test-turn%)
  )
