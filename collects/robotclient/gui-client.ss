(module gui-client mzscheme
  (require "board.ss"
           (lib "class.ss")
           (lib "mred.ss" "mred")
           "drawboard.ss")
  
  (provide initialize update)
  
  (define drawn (make-parameter #f))
  
  (define (initialize width height board robots)
    (let* ((board (make-board width height board))
           (f (instantiate frame% ("Robot"))))
      (drawn (instantiate board-panel% (f width height board)))
      (send (drawn) install-robots&packages
            ;; Each robot is (list id x y money max-lift (list pkg-id ...))
            (map
             (lambda (r)
               (list (robot-id r) (robot-x r) (robot-y r) (player-money) (player-capacity)
                     (map package-id (packages-held))))
             robots)
            ;; Each package is (list id x y dest-x dext-y weight)
            '())
      (send f show #t)))
  
  
  (define (trans-spot spot)
    (case (get-type spot)
      ((0) 'plain)
      ((1) 'water)
      ((2) 'wall)
      ((3) 'base)))
  
  (define (make-board w h b)
    (list->vector
     (let loop ((i 1))
       (cond
         ((> i h) null)
         (else
          (cons
           (list->vector
            (let loop ((j 1))
              (cond
                ((> j w) null)
                (else
                 (cons (trans-spot (get-spot b j i)) (loop (add1 j)))))))
           (loop (add1 i))))))))
               
  
  (define (update actions)
    (send (drawn) queue-robot-actions
          ;; Each robot action is (list id bid (one-of 'e 'w 'n 's (list 'pick id...) (list 'drop id ...)))
          actions)
    (send (drawn) apply-queued-actions))
  
  )