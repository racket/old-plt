
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
               
  (define f (instantiate frame% ("Robot")))
  (define drawn (instantiate board-panel% (f width height board)))
  
  (send drawn install-robots&packages
        ;; Each robot is (list id x y (list pkg-id ...))
        '((1 2 2 ())
          (2 7 4 ())
          (3 8 8 (3))
          (4 2 1 ())
          (5 9 2 ())
          (6 9 3 ())
          (7 6 2 ())
          (8 6 3 ()))
        ;; Each package is (list id x y dest-x dext-y weight)
        '((3 8 8 7 7 20)
          (2 9 2 3 6 80)
          (1 5 5 4 4 100)))
  
  (send drawn queue-robot-actions
        ;; Each robot action is (list id (one-of 'e 'w 'n 's (list 'pick id...) (list 'drop id ...)))
        '((2 s)
          (5 (pick 2))
          (3 (drop 3))
          (7 n)
          (6 w)
          (8 e)))
        
  (make-object button% "Move" f (lambda (b e) (send drawn apply-queued-actions)))
  
  (send f show #t))

      