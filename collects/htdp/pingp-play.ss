(require-library "pingp-sig.ss" "htdp")
(require-library "ping-play-unit.ss" "htdp")

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    [PINGP : pingpDrawS ((require-unit/sig "pingp.ss") PLT)]
    [BALL  : ballS      (ping-play-U PINGP PLT)]
    (GO : goS ((unit/sig goS (import ballS pingpS)
		 (define (go s)
		   (printf "Have fun playing, ~a~n" s)
		   (play make-ball make-speed ball-posn move-in-box)))
	       BALL
	       (PINGP : pingpS))))
  (export (var (PINGP change-speed))
          (var (PINGP change-wind))
	  (var (PINGP change-width))
	  (var (PINGP change-height))
          (open GO)))
