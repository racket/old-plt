(require (lib "plot.ss" "plplot"))

(plot 
 '((x-max 30) (y-max 30)) 
 (points (map (lambda (x) (make-posn x (random x))) (build-list 30 (lambda (x) (add1 x))))))