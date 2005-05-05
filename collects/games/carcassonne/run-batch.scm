(module run-batch mzscheme 

  ;; run a simple game with "dumb" players; optionally: display it on the screen 

  (require "run-aux.scm" "player.scm" (lib "mred.ss" "mred"))

  (define names '("Matthias" "Matthew" "Robby" "Shriram"))

  (define th (go (map (lambda (x) (list player% x)) names) #f))
  
  (yield th))

