
(require (lib "plot.ss" "plplot"))

(define x-vals (build-list 15 (lambda (x) x) ))
(define errors (build-list 15 (lambda (x) 1)))

(define (fun x)
  (* 3 (exp (* x -1 1.32))))
(define z-vals (map fun x-vals))
        
         
(define (gues-fun x y a b)
  (* a (exp (* x -1 b))))

(define params (fit
 gues-fun
 x-vals
 x-vals
 z-vals
 errors
 (list 1 1)))

(plot (mix* 
       (points (map vector x-vals z-vals))
       (line (lambda (x)
               (apply gues-fun x 0 params))))
      (x-min -1) (x-max 20)
      (y-min -1) (y-max 10))
 