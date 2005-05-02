(module test-regions mzscheme 

  ;; predefine regions for tests

  (require "graph.scm"
           "tiles.scm"
           "view.scm"
           (lib "class.ss"))
  
  (provide road0 road1 road2 road3
           castle1 castle2 castle3 castle5
           abbey1 abbey2 abbey3
           
           ;; GraphMaker = (->* graph<%> tile<%)
           
           place-follower
           ;; GraphMaker Number Number Position Follower -> GraphMaker 
           ;; add a follower at (x,y) in dir and return new graph 
           )
  
  (define (place-follower comp x y d f)
    (lambda ()
      (define-values (g lt) (comp))
      (define zero (send g lookup x y))
      (send zero place-follower! d f)
      (values g lt)))
  
  (define (road0)
    ;; complete a road by bridging a gap between two tiles
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "2" -1 0 270))
    (send graph0 insert-tile! (make-tile "1" 0 1 0))
    (send graph0 insert-tile! (make-tile "8" 1 1 90))
    (send graph0 insert-tile! (make-tile "8" 2 1 180))
    (send graph0 insert-tile! (make-tile "2" 2 0 90))
    (send graph0 insert-tile! (make-tile "21" 1 0 90))
    (values graph0 (make-tile "21" 1 0 90)))
  
  
  (define (road1) 
    ;; complete a single road with a finish tile 
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "1" 0 1 90))
    (send graph0 insert-tile! (make-tile "2" -1 0 270))
    (send graph0 insert-tile! (make-tile "19" 1 0 0))
    (values graph0 (make-tile "19" 1 0 0)))
  
  
  (define (road2)
    ;; complete two roads with a fork in the road
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "2" -1 0 270))
    (send graph0 insert-tile! (make-tile "1" 0 1 0))
    (send graph0 insert-tile! (make-tile "14" 0 2 90))
    (send graph0 insert-tile! (make-tile "20" 1 0 0))
    (send graph0 insert-tile! (make-tile "22" 1 1 0))
    (send graph0 insert-tile! (make-tile "20" 1 2 270))
    (send graph0 insert-tile! (make-tile "7" 1 3 180))
    (send graph0 insert-tile! (make-tile "17" 2 1 180))
    (send graph0 insert-tile! (make-tile "19" 2 0 0))
    (values graph0 (make-tile "19" 2 0 0)))
  
  
  (define (road3)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "22" 0 1 0))
    (send graph0 insert-tile! (make-tile "22" 1 1 90))
    (send graph0 insert-tile! (make-tile "22" 1 2 180))
    (send graph0 insert-tile! (make-tile "22" 0 2 270))
    (values graph0 (make-tile "22" 0 2 270)))
  
  (define (castle1)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "10" 0 -1 180))
    (map (lambda (t) (printf "~s~n" (send t snip)))
         (send graph0 potential-locations-for-tile "3"))
    (values graph0 (make-tile "10" 0 -1 180)))
  
  
  (define (castle2)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "1" 0 1 0))
    (send graph0 insert-tile! (make-tile "3" 0 -1 0))
    (send graph0 insert-tile! (make-tile "14" -1 -1 0))
    (send graph0 insert-tile! (make-tile "14" 0 -2 90))
    (send graph0 insert-tile! (make-tile "14" 1 -1 180))
    (values graph0 (make-tile "14" 1 -1 180)))
  
  (define (castle3)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "8" 0 -1 90))
    (send graph0 insert-tile! (make-tile "10" 1 0 0))
    (send graph0 insert-tile! (make-tile "14" 1 -1 180))
    (values graph0 (make-tile "14" 1 -1 180)))
  
  
  (define (castle5)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "8" 0 -1 90))
    (send graph0 insert-tile! (make-tile "4" 1 -1 0))
    (send graph0 insert-tile! (make-tile "8" 1 -2 90))
    (send graph0 insert-tile! (make-tile "8" 2 -1 270))
    (send graph0 insert-tile! (make-tile "14" 2 -2 180))
    (values graph0 (make-tile "14" 2 -2 180)))
  
  
  (define (abbey1)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "1" 0 1 0))
    (send graph0 insert-tile! (make-tile "6" 1 0 90))
    (send graph0 insert-tile! (make-tile "8" 1 1 0))
    (send graph0 insert-tile! (make-tile "9" 1 2 90))
    (send graph0 insert-tile! (make-tile "9" 0 2 180))
    (send graph0 insert-tile! (make-tile "9" -1 2 90))
    (send graph0 insert-tile! (make-tile "14" -1 1 270))
    (send graph0 insert-tile! (make-tile "11" -1 0 180))
    (values graph0 (make-tile "11" -1 0 180)))
  
  (define (abbey2)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "1" 0 1 0))
    (send graph0 insert-tile! (make-tile "2" 1 0 90))
    (send graph0 insert-tile! (make-tile "4" -1 1 270))
    (send graph0 insert-tile! (make-tile "5" -1 2 270))
    (send graph0 insert-tile! (make-tile "6" -1 0 270))
    (send graph0 insert-tile! (make-tile "8" 0 2 90))
    (send graph0 insert-tile! (make-tile "8" 1 2 180))
    (send graph0 insert-tile! (make-tile "9" 2 2 0))
    (send graph0 insert-tile! (make-tile "9" 2 1 90))
    (send graph0 insert-tile! (make-tile "9" 2 0 0))
    (send graph0 insert-tile! (make-tile "1" 1 1 90))
    (values graph0 (make-tile "1" 1 1 90)))
  
  
  (define (abbey3)
    (define graph0 (new graph%))
    (send graph0 insert-tile! (make-tile "1" 0 1 0))
    (send graph0 insert-tile! (make-tile "1" 1 1 90))
    (send graph0 insert-tile! (make-tile "1" 1 2 180))
    (send graph0 insert-tile! (make-tile "1" 0 2 270))
    (send graph0 insert-tile! (make-tile "2" 1 0 90))
    (send graph0 insert-tile! (make-tile "2" 2 0 270))
    (send graph0 insert-tile! (make-tile "2" 2 2 0))
    (send graph0 insert-tile! (make-tile "16" 2 1 90))
    (values graph0 (make-tile "16" 2 1 90)))
  
  (define (bug1)
    (let* ((graph0 (new graph%))
           (graph0 (send graph0 insert-tile (make-tile "7" 1 0 90)))
           (graph0 (send graph0 place-follower (send graph0 lookup 1 0) -100 "black"))
           (graph0 (send graph0 insert-tile (make-tile "7" 2 0 180)))
           (graph0 (send graph0 place-follower (send graph0 lookup 1 0) 270 "cyan"))
           ;(graph0 (send graph0 insert-tile (make-tile "15" 1 1 180)))
           ;(graph0 (send graph0 place-follower (send graph0 lookup 2 0) -100 "yellow"))
           #|
         (graph0 (send graph0 insert-tile (make-tile "8" 0 1 180)))
         (graph0 (send graph0 insert-tile (make-tile "20" 3 0 270)))
         (graph0 (send graph0 insert-tile (make-tile "24" -1 0 180)))
         (graph0 (send graph0 insert-tile (make-tile "15" 1 2 180)))
         (graph0 (send graph0 insert-tile (make-tile "8" 0 -1 180)))
         (graph0 (send graph0 place-follower (send graph0 lookup 1 2) 180 "red"))
         (graph0 (send graph0 insert-tile (make-tile "20" 3 -1 270)))
         (graph0 (send graph0 place-follower (send graph0 lookup 0 -1) -100 "black"))
         (graph0 (send graph0 insert-tile (make-tile "12" 0 2 270)))
         (graph0 (send graph0 place-follower (send graph0 lookup -1 0) 0 "cyan"))
         (graph0 (send graph0 insert-tile (make-tile "3" 0 3 0)))
         (graph0 (send graph0 insert-tile (make-tile "17" -1 2 180)))
         (graph0 (send graph0 place-follower (send graph0 lookup 0 3) -100 "red"))
         (graph0 (send graph0 insert-tile (make-tile "12" 2 1 90)))
         (graph0 (send graph0 insert-tile (make-tile "15" 1 -1 0)))
         (graph0 (send graph0 place-follower (send graph0 lookup -1 2) 180 "cyan"))
         (graph0 (send graph0 insert-tile (make-tile "10" 4 -1 180)))
         (graph0 (send graph0 insert-tile (make-tile "12" 4 0 90)))
         (graph0 (send graph0 place-follower (send graph0 lookup 4 -1) 0 "red"))
         |#
           )
      (values graph0 '_)))
  
  (define (bug1.1)
    (let* ((graph0 (new graph%))
           (graph0 (send graph0 insert-tile (make-tile "7" 1 0 90)))
           (graph0 (send graph0 insert-tile (make-tile "7" 2 0 180)))
           (graph0 (send graph0 insert-tile (make-tile "10" 1 1 0)))
           (graph0 (send graph0 insert-tile (make-tile "10" 1 -1 180)))
           ;(graph0 (send graph0 place-follower (send graph0 lookup 2 0) -100 "black"))
           ;(graph0 (send graph0 insert-tile (make-tile "15" 1 1 180)))
           ;(graph0 (send graph0 place-follower (send graph0 lookup 2 0) -100 "yellow"))
           #|
         (graph0 (send graph0 insert-tile (make-tile "8" 0 1 180)))
         (graph0 (send graph0 insert-tile (make-tile "20" 3 0 270)))
         (graph0 (send graph0 insert-tile (make-tile "24" -1 0 180)))
         (graph0 (send graph0 insert-tile (make-tile "15" 1 2 180)))
         (graph0 (send graph0 insert-tile (make-tile "8" 0 -1 180)))
         (graph0 (send graph0 place-follower (send graph0 lookup 1 2) 180 "red"))
         (graph0 (send graph0 insert-tile (make-tile "20" 3 -1 270)))
         (graph0 (send graph0 place-follower (send graph0 lookup 0 -1) -100 "black"))
         (graph0 (send graph0 insert-tile (make-tile "12" 0 2 270)))
         (graph0 (send graph0 place-follower (send graph0 lookup -1 0) 0 "cyan"))
         (graph0 (send graph0 insert-tile (make-tile "3" 0 3 0)))
         (graph0 (send graph0 insert-tile (make-tile "17" -1 2 180)))
         (graph0 (send graph0 place-follower (send graph0 lookup 0 3) -100 "red"))
         (graph0 (send graph0 insert-tile (make-tile "12" 2 1 90)))
         (graph0 (send graph0 insert-tile (make-tile "15" 1 -1 0)))
         (graph0 (send graph0 place-follower (send graph0 lookup -1 2) 180 "cyan"))
         (graph0 (send graph0 insert-tile (make-tile "10" 4 -1 180)))
         (graph0 (send graph0 insert-tile (make-tile "12" 4 0 90)))
         (graph0 (send graph0 place-follower (send graph0 lookup 4 -1) 0 "red"))
         |#
           )
      (values graph0 '_)))
  
  (provide bug1 bug1.1)

  )
