
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
        ;; Each robot is (list id x y money max-lift (list pkg-id ...))
        '((1 2 2 1000 100 (7))
          (2 7 4 1000 120 (4 5))
          (3 8 8 1000 50 (3))
          (4 2 1 1000 60 ())
          (5 9 2 1000 80 ())
          (6 9 3 1000 90 ())
          (7 6 2 40 120 ())
          (8 6 3 20 60 ()))
        ;; Each package is (list id x y dest-x dext-y weight)
        '((3 8 8 7 7 20)
          (2 9 2 3 6 80)
          (1 5 5 4 4 100)
          (4 7 4 7 3 100)
          (5 7 4 3 2 60)
          (7 2 2 1 1 20)))
  
  (send drawn queue-robot-actions
        ;; Each robot action is (list id bid (one-of 'e 'w 'n 's (list 'pick id...) (list 'drop id ...)))
        '((2 10 s)
          (5 10 (pick 2))
          (3 10 (drop 3))
          (7 10 n)
          (6 10 w)
          (8 10 e)
          (1 1 e)))
        
  (define times 0)
  
  (make-object button% "Move" f (lambda (b e) 
                                  (set! times (add1 times))
                                  (send drawn apply-queued-actions)
                                  (send drawn queue-robot-actions
                                        `((5 10 w)
                                          (3 10 (drop 3))
                                          (7 10 n)
                                          (6 10 w)
                                          ,@(if (< times 2)
                                                '((1 1 s))
                                                null)
                                          ,@(if (< times 3)
                                                '((8 10 e))
                                                null)))))
  
  (send f show #t))

      