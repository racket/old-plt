(require (lib "plot.ss" "plplot"))


(define (trig x y) (* (sin x) (sin y)))
(plot '((x-min -1.5) (x-max 1.5) (y-min -1.5) (y-max 1.5) 
        (title "shdade of F(x,y) = sin(x) * sin(y)"))     
      (shade trig))