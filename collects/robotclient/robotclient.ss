
(module robotclient mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "unit.ss")
           (lib "list.ss")
           "drawboard.ss")

  (define width 10)
  (define height 10)
  (define board #(#(water water water water water water water water water water)
                   #(plain plain plain plain plain plain plain plain plain plain)
                   #(plain plain plain plain plain plain plain plain plain plain)
                   #(plain plain plain plain plain plain plain plain plain plain)
                   #(plain plain plain plain plain plain plain plain plain plain)
                   #(wall  wall  plain plain plain plain plain plain plain plain)
                   #(plain plain plain plain plain plain plain plain plain plain)
                   #(plain plain plain plain plain plain plain plain base  plain)
                   #(plain plain plain plain plain plain plain plain plain plain)
                   #(plain plain plain plain plain plain plain plain plain plain)
                   #(plain plain plain plain plain plain plain plain plain plain)))
               
  (define f (instantiate frame% ("Robot") [style '(no-resize-border)]))
  (define canvas (instantiate board-canvas% (f width height board)))
  
  (send canvas install-packages '((3 8 8 7 7 20)
                                  (1 5 5 4 4 100)))
  (send canvas install-robots '((1 2 2)
                                (2 7 4)
                                (3 4 4)
                                (4 2 1)
                                (5 9 2)
                                (6 9 3)
                                (7 6 2)
                                (8 6 3)))
  
  (send f show #t))

      