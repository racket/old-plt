(require-library "pingp-sig.ss" "htdp")
(require-library "protect-play-unit.ss" "htdp")
(require-library "ping-play-unit.ss" "htdp")

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    [PINGP : pingpDrawS ((require-unit/sig "pingp.ss") PLT)]
    [BALL  : ping-protS (ping-play-U PINGP PLT)]
    [PROT  : protectS   (protect-play-U BALL PINGP PLT)]
    (GO : goS ((unit/sig goS (import protectS pingpS)
		 (define n (+ 10 (random 10)))
		 (define (go s)
		   (set! n (+ 10 (random 10)))
		   (printf "You're facing ~a balls. Have fun playing, ~a~n" n s)
		   (protect (mk-balls n)
		            move-balls
			    remove-balls-hit-paddle
			    remove-outside-balls
			    balls-posn)))
	       PROT
	       (PINGP : pingpS))))
  (export (var (PINGP change-speed))
          (var (PINGP change-wind))
	  (var (PINGP change-width))
	  (var (PINGP change-height))
          (open GO)))
