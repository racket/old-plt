;; Signature for pingp-lib:

(reference-file "draw-sig.ss")

(define-signature pingpS
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
    NORTH SOUTH EAST WEST
    PADDLE-X
    PADDLE-Y
    ))

(define-signature pingpDrawS ((open pingpS) (open drawS)))
