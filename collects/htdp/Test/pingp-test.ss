;; More primitives on posn:
;; ------------------------
;; posn+: posn posn -> posn (component-wise addition)
(define (posn+ p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

;; posn+: number posn -> posn (component-wise muliplication)
(define (posn*s f p)
  (make-posn (* f (posn-x p)) (* f (posn-y p))))

;; balls = (cons posn (cons posn null))
;;   the first posn is the position, 
;;   the second one the speed per time unit

;; The ball representation and some abasic primitives:
;; ---------------------------------------------------
(define (make-ball pos speed) (cons pos (cons speed null)))
(define (ball-posn ball) (first ball))
(define (ball-speed ball) (first (rest ball))) 

;; ns-direction : ball => {'NORTH, 'SOUTH, 'none}
(define (ns-direction ball)
  (cond
    ((< (posn-y (ball-speed ball)) 0) 'NORTH)
    ((> (posn-y (ball-speed ball)) 0) 'SOUTH)))

;; ew-direction : ball => {'EAST, 'WEST, 'none}
(define (ew-direction ball)
  (cond
    ((< (posn-x (ball-speed ball)) 0) 'WEST)
    ((> (posn-x (ball-speed ball)) 0) 'EAST)))

;; ns-dist-to-wall : ball -> number (dist to n/s wall moving towards)
(define (ns-dist-to-wall ball)
  (cond
    ((eq? (ns-direction ball) 'NORTH) (- (posn-y (ball-posn ball)) NORTH))
    ((eq? (ns-direction ball) 'SOUTH) (- SOUTH (posn-y (ball-posn ball))))))

;; ew-dist-to-wall : ball -> number (dist to e/w wall moving towards)
(define (ew-dist-to-wall ball)
  (cond
    ((eq? (ew-direction ball) 'WEST) (- (posn-x (ball-posn ball)) WEST))
    ((eq? (ew-direction ball) 'EAST) (- EAST (posn-x (ball-posn ball))))))

;; ew-time-to-wall : ball -> number (time before ew wall is hit)
(define (ns-time-to-wall ball)
  (/ (ns-dist-to-wall ball) (abs (posn-y (ball-speed ball)))))

;; ns-time-to-wall : ball -> number (time before ns wall is hit)
(define (ew-time-to-wall ball)
  (/ (ew-dist-to-wall ball) (abs (posn-x (ball-speed ball)))))

;; Moving a Ball
;; -------------
;; move-in-box : ball number -> ball
(define (move-in-box ball t)
  (cond
    ((or (eq? (bounces-from ball t) 'NORTH) (eq? (bounces-from ball t) 'SOUTH))
     (move-in-box (ns-bounce (move-ball ball (ns-time-to-wall ball)))
		  (- t (ns-time-to-wall ball))))
    ((or (eq? (bounces-from ball t) 'EAST) (eq? (bounces-from ball t) 'WEST))
     (move-in-box (ew-bounce (move-ball ball (ew-time-to-wall ball)))
		  (- t (ew-time-to-wall ball))))
    (else (move-ball ball t))))

(define (b x y) y)

;; bounces-from : ball number -> {'NORTH, 'SOUTH, 'EAST, 'WEST, 'none}
;; version 0: for bouncing balls
(define (bounces-from/version0 ball t)
  (cond
    ((and (<= (ns-time-to-wall ball) (ew-time-to-wall ball))
	  (<= (ns-time-to-wall ball) t))
     (ns-direction ball))
    ((and (<= (ew-time-to-wall ball) (ns-time-to-wall ball))
	  (<= (ew-time-to-wall ball) t))
     (ew-direction ball))
    (else 'none)))

;; version 1: for playing games
(define (bounces-from ball t)
  (cond
    ((and (<= (ns-time-to-wall ball) (ew-time-to-wall ball))
	  (<= (ns-time-to-wall ball) t))
     (ns-direction ball))
    ((and (<= (ew-time-to-wall ball) (ns-time-to-wall ball))
	  (<= (ew-time-to-wall ball) t))
     (cond 
       ((landed-on-paddle? (ball-posn (move-ball ball (ew-time-to-wall ball))))
	(ew-direction ball))
       (else 'none)))
    (else 'none)))

;; move : ball number -> ball
(define (move-ball ball t)
  (make-ball (posn+ (ball-posn ball) (posn*s t (ball-speed ball)))
	     (ball-speed ball)))

;; ns-bounce : ball -> ball
(define (ns-bounce ball)
  (make-ball (ball-posn ball)
	     (make-posn (posn-x (ball-speed ball)) 
			(- (posn-y (ball-speed ball))))))

;; ew-bounce-west : ball -> ball
(define (ew-bounce ball)
  (make-ball (ball-posn ball)
             (make-posn (- (posn-x (ball-speed ball)))
		           (posn-y (ball-speed ball)))))
