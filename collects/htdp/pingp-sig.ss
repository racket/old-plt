(require-library "draw-sig.ss" "htdp")

;; to be provided to student
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
    NORTH SOUTH EAST WEST FAR-WEST 
    PADDLE-X
    PADDLE-Y
    ))

(define-signature pingpDrawS
  ((open pingpS) (open drawS)))

;; needed from ping-play-unit for playing ping-pong
(define-signature ping-protS-core
  (make-ball make-speed ball-posn))
(define-signature ping-protS-extr
  (ns-bounce ns-time-to-wall ew-time-to-wall move-ball))
(define-signature ballS
  ((open ping-protS-core) move-in-box))

;; needed from ping-play-unit for playing protect-the-wall
(define-signature ping-protS
  ((open ping-protS-core) (open ping-protS-extr)))

;; provided by ping-play-unit
(define-signature ppu-S
  ((open ping-protS-core) (open ping-protS-extr) move-in-box))

;; provided by protect-play-unit
(define-signature protectS
  (mk-balls move-balls remove-balls-hit-paddle remove-outside-balls balls-posn))

;; provided by the glue units
(define-signature goS
  (go))
