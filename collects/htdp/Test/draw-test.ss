;; TeachPack: draw.ss
;; Language: Beginner 

(start 200 200)

(define L1 (list (make-posn 10 10) (make-posn 20 80)))
(define L2 (list (make-posn 20 80) (make-posn 100 20) 'blue))
(define R (list (make-posn 20 80) 10 10 'red))
(define D (list (make-posn 33 33) 10 'yellow))
(define C (list (make-posn 88 10) 17 'green))

(apply draw-circle C)
(apply draw-solid-line L1)
(apply draw-solid-line L2)
(apply draw-solid-rect R)
(apply draw-solid-disk D)

(sleep-for-a-while 1)
(apply clear-circle C)

(sleep-for-a-while 1)
(apply clear-solid-disk D)

(sleep-for-a-while 1)
(apply clear-solid-rect R) 

(sleep-for-a-while 1)
(apply clear-solid-line L2)

(sleep-for-a-while 1)
(apply clear-solid-line L1)

(clear-all)
