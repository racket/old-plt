(require (lib "renderers.ss" "plplot"))
(require (lib "view.ss" "plplot"))
(require (lib "plot.ss" "plplot"))

(instantiate 2d-view% ((line (lambda (x) x))))

(define r1 (line (lambda (x) x)))

(define r2 (line (lambda (x) (sin x))))

(define r3 (line (lambda (x) (* x x))))

(define r1r2 (mix r1 r2))

(define all (mix* r1 r2 r3))  

(instantiate 2d-view% (all))

(plot (line (lambda (x) (* x x))))
(plot (line (lambda (x) (sin x))) 
      (x-min -1.5) (x-max 1.5) (y-min -1) (y-max 1))

(plot all 
      (x-min -1.5) (x-max 1.5) (y-min -1) (y-max 1))