#cs(module graphing2 mzscheme
  (require (lib "error.ss" "htdp")
	   (lib "unitsig.ss")
           (lib "prim.ss" "lang")
           (lib "plot.ss" "plot")
           (lib "renderer-helpers.ss" "plot"))
     
     (provide 
      graph-fun
      graph-line
      )
     
     
     (define *X-MIN* -5)
     (define *X-MAX* 5)
     
     (define *X-STEP* .1)
     
     (define *X-SAMP* (inexact->exact (truncate (/ (+ 1 (- *X-MAX* *X-MIN*)) *X-STEP*))))
     
     (define *Y-MIN* -5)
     (define *Y-MAX* 5)
     
     (define-higher-order-primitive graph-line graph-line/proc (f _))
     (define-higher-order-primitive graph-fun graph-fun/proc (f _))
  
     
     
     
     ;; check : tst tst tst -> void
     (define (check tag f color)
       (check-proc tag f 1 '1st "one argument")
       (check-arg tag (symbol? color) 'symbol '2nd color))
     
     ;; (num -> num) symbol -> VIEW%
     ;; effect: draw function graph for x in [-5,5] at delta = .1
     (define (graph-line/proc f col)
       (check 'graph-line f col)
       (plot (line f (color col)) 
             (x-min *X-MIN*) (x-max *X-MAX*)
             (y-min *Y-MIN*) (y-max *Y-MAX*)))
             
     
     ;; (num -> num) symbol -> VIEW%
     ;; effect: draw function graph for x in [-5,5] at delta = .1
     (define (graph-fun/proc f col)
       (check 'graph-fun f col)
       (plot (points 
              (map (lambda (x) (vector x (f x))) 
                   (x-values *X-SAMP* *X-MIN* *X-MAX*))
              (color col)
              (sym 'bullet))
             (x-min *X-MIN*) (x-max *X-MAX*) 
             (y-min *Y-MIN*) (y-max *Y-MAX*)))
     
     )
   
  
   