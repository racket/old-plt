(require-library "pingp-sig.ss")

(define-signature ballS
  (make-ball ball-posn ball-speed make-speed move-in-box))

(define-signature goS
  (go))

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    [PINGP : pingpDrawS ((require-unit/sig "pingp-lib.ss") PLT)]
    [BALL : ballS
      ((unit/sig ballS
	 (import pingpDrawS plt:userspace^)
	 (require-library "pingp-test-play.ss" "htdp"))
       PINGP PLT)]
    (GO : goS
      ((unit/sig goS (import ballS pingpS)
	 (define (go s)
	   (printf "Have fun playing, ~a~n" s)
	   (play make-ball make-speed ball-posn move-in-box)))
       BALL (PINGP : pingpS))))
  (export (var (PINGP change-speed))
          (var (PINGP change-wind))
	  (var (PINGP change-width))
	  (var (PINGP change-height))
          (open GO)))
