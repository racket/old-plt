(require (lib "plot.ss" "plplot"))

(plot '() 
      (line (lambda (x) x) '((color red))))