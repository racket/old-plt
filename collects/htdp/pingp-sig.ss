(module pingp-sig mzscheme
  (require (lib "unitsig.ss")
           "draw-sig.ss")
  (provide pingp^)
  
  ;; to be provided to student
  (define-signature pingp^
    ( play
      ; ((posn posn -> ball) (ball -> posn) (ball -> posn) (ball -> ball) -> void)
      ;  make-ball           ball-posn      ball-speed     move/move-in-box
      landed-on-paddle?
      protect
      ; ((balls: (listof ball)
      ;  (move-balls : (listof ball) -> (listof posn))
      ;  (balls-posns :(listof ball) -> (listof ball))
      ;  (balls-destroyed : (listof ball) -> (listof ball))) -> void)
      trace
      trace-ball
      change-width
      change-height
      change-speed
      change-wind
      NORTH SOUTH EAST WEST FAR-WEST 
      PADDLE-X
      PADDLE-Y
      ))
  
  (define-signature pingpDrawS
    ((open pingp^) (open draw^)))
  
  ;; needed from ping-play-unit for playing ping-pong
  (define-signature ping-prot-core^
    (make-ball make-speed ball-posn))
  (define-signature ping-prot-extr^
    (ns-bounce ns-time-to-wall ew-time-to-wall move-ball))
  (define-signature ball^
    ((open ping-prot-core^) move-in-box))
  
  ;; needed from ping-play-unit for playing protect-the-wall
  (define-signature ping-prot^
    ((open ping-prot-core^)
     (open ping-prot-extr^)))
  
  ;; provided by ping-play-unit
  (define-signature ppu^
    ((open ping-prot-core^) 
     (open ping-prot-extr^)
     move-in-box))
  
  ;; provided by protect-play-unit
  (define-signature protect^
    (mk-balls move-balls remove-balls-hit-paddle remove-outside-balls balls-posn))
  
  ;; provided by the glue units
  (define-signature go^
    (go)))
  