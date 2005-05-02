#cs
(module test-player mzscheme
  
  (require "player.scm"
           (lib "class.ss"))
  
  (printf "testing player ... ~n")
  
  (define p (new player% [name "mf"]))
  (define t (new test-turn%))

  (send p take-turn t)
  
  (printf "... done~n")
  
  )