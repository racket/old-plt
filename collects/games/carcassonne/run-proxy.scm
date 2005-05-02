#cs
(module run-proxy mzscheme 
  
  ;; run a bunch of players in "distributed" mode, but locally 

  (require "if.scm" "run-proxy-aux.scm")
  
  (REG-WAIT 10) ;; seconds
  (max-turn-time 24)
  (define names '("Matthias" "Matthew" "Robby" "Shriram"))
  (define stop (go names #f)) ;; #t for view 
  )