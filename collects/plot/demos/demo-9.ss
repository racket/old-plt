(require (lib "plot.ss" "plplot"))

(define (trig x y) (* (sin x) (sin y)))
(define a (plot  (contour trig)))
(define b (plot  (shade trig)))
(mix b a)