#cs
(module run-proxy-bad mzscheme 
  
  ;; run a bunch of "locally bad" players in "distributed" mode
  ;; this is analogous to run-bad-batch.scm 
  ;; comment out bad player at will 
  
  (require "run-proxy-aux.scm" "player.scm")

  (define stop (go (list 
                    (list "crash on second inform" player-bad5%)
                    (list "try to place two followers" player-bad4%)
                    (list "try to place a follower before tile" player-bad3%)
                    (list "try to place two tiles" player-bad2%)
                    (list "won't even place a tile" player-bad1%)
                    (list "crashes" player-bad0%)
                    "Matthias"
                    "Matthew"
                    "Robby"
                    "Shriram")
                   #f))
  )
