(require (lib "plot.ss" "plplot"))

(define (trig x y) (* (sin x) (sin y)))
(plot (mix*
       (shade trig)
       (contour trig)
       (field (gradient trig ) '((samples 25))))
      (x-min -1.5) (x-max 1.5) (y-min -1.5) (y-max 1.5) 
      (title "gradient field +shdade + contours of F(x,y) = sin(x) * sin(y)"))