(require (lib "plot.ss" "plplot"))

(plot '() 
      (line (lambda (x) (sin x))))