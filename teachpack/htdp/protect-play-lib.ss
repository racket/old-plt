(reference-file "pingp-sig.ss")

(define-signature ballS
  (mk-balls move-balls remove-balls-hit-paddle remove-outside-balls balls-posn))
(define-signature goS
  (go))
(define-signature playS
  (make-ball ball-speed make-speed speed-x bounces-from ns-bounce move-ball 
	     ns-time-to-wall ew-time-to-wall ball-posn))

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    [PINGP : pingpDrawS ((require-unit/sig "pingp-lib.ss") PLT)]
    [PLAY  : playS  ((unit/sig playS (import pingpDrawS plt:userspace^) (include "pingp-test-play.ss"))
		     PINGP PLT)]
    [BALL : ballS   ((unit/sig ballS (import playS pingpDrawS plt:userspace^) (include "protect-test.ss"))
		     PLAY PINGP PLT)]
    (GO : goS
      ((unit/sig goS (import ballS pingpS)
	 (define n (+ 10 (random 10)))
	 (define (go s)
 	   (printf "You're facing ~a balls. Have fun playing, ~a~n" n s)
	   (protect (mk-balls n)
	            move-balls
		    remove-balls-hit-paddle
		    remove-outside-balls
		    balls-posn)))
       BALL (PINGP : pingpS))))
  (export (var (PINGP change-speed))
          (var (PINGP change-wind))
	  (var (PINGP change-width))
	  (var (PINGP change-height))
          (open GO)))
