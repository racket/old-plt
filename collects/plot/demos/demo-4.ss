(require (lib "plot.ss" "plplot"))

(plot '((title "gradient field of F(x,y) = sin(x) * sin(y)"))
      (field (gradient (lambda (x y) (* (sin x) (cos y)))) '((samples 25))))