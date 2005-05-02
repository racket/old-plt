;; run a simple game with "dumb" and broken players
(module run-bad-batch mzscheme 
  
  ;; run the ordinary game with a bunch of bad players 

  (require "run-aux.scm" "player.scm")
  
  (define l
    (list (list player-bad5% "crash on second inform")
          (list player-bad4% "try to place two followers")
          (list player-bad3% "try to place a follower before tile")
          (list player-bad2% "try to place two tiles")
          (list player-bad1% "won't even place a tile")
          (list player-bad0% "crashes")
          (list player% "Matthias")
          (list player% "Matthew")
          (list player% "Robby")
          (list player% "Shriram")
          ))
  
  (define th (go l #f)))
