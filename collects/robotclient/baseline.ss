(module baseline mzscheme
  (require "board.ss"
           (lib "list.ss"))
  (provide compute-baseline-move)
  
  (define (compute-drops)
    (filter (lambda (x) x)
            (map
             (lambda (p)
               (cond
                 ((and (= (get-player-x) (package-x p))
                       (= (get-player-y) (package-y p)))
                  (package-id p))
                 (else #f)))
             (packages-held))))
               
  (define (compute-picks packages)
    (let loop ((remaining-weight (- (player-capacity)
                                    (apply + (map package-weight (packages-held)))))
               (picks (mergesort packages (lambda (a b)
                                            (< (package-weight a)
                                               (package-weight b))))))
        (cond
          ((null? picks) null)
          ((< (package-weight (car picks)) remaining-weight)
           (cons (package-id (car picks))
                 (loop (- remaining-weight (package-weight (car picks)))
                       (cdr picks))))
          (else (loop remaining-weight (cdr picks))))))
          
  (define (dist gx gy px py)
    (+ (abs (- px gx))
       (abs (- py gy))))
  
  (define (compute-goal px py)
    (cond
      ((null? (packages-held))
       (let loop ((x +inf.0)
                  (y +inf.0)
                  (i 1)
                  (j 1))
         (cond
           ((> i (board-width)) (cons x y))
           ((> j (board-height)) (loop x y (add1 i) 1))
           ((and (= 3 (get-type (get-spot (board) j i)))
                 (< (dist j i px py)
                    (dist x y px py)))
            (loop j i i (add1 j)))
           (else (loop x y i (add1 j))))))
      (else
       (let loop ((x 1)
                  (y 1)
                  (max -inf.0)
                  (p (packages-held)))
         (cond
           ((null? p) (cons x y))
           (else
            (let ((score (/ (package-weight (car p))
                            (dist (package-x (car p))
                                  (package-y (car p))
                                  px
                                  py))))
              (cond
                ((< max score) (loop (package-x (car p))
                                     (package-y (car p))
                                     score
                                     (cdr p)))
                (else (loop x y max (cdr p)))))))))))
            
            
                  
  (define (best-dir px py gx gy)
    (cond
      ((< px gx) 'e)
      ((> px gx) 'w)
      ((> py gy) 's)
      (else 'n)))
  
  
  (define (compute-baseline-move packages robots)
    (let ((drops (compute-drops)))
      (cond
        ((not (null? drops)) (make-command 1 'd drops))
        (else
         (let ((picks (compute-picks packages)))
           (cond
             ((not (null? picks)) (make-command 1 'p picks))
             (else
              (let ((goal (compute-goal (get-player-x) (get-player-y))))
                (let loop ((dir (best-dir (get-player-x)
                                          (get-player-y)
                                          (car goal)
                                          (cdr goal))))
                  (let ((dest (case dir
                                ((e) (cons (add1 (get-player-x))
                                           (get-player-y)))
                                ((w) (cons (sub1 (get-player-x))
                                           (get-player-y)))
                                ((n) (cons (get-player-x)
                                           (add1 (get-player-y))))
                                ((s) (cons (get-player-x)
                                           (sub1 (get-player-y)))))))
                    (cond
                      ((or (= 1 (get-type (get-spot (board) (car dest) (cdr dest))))
                           (= 2 (get-type (get-spot (board) (car dest) (cdr dest)))))
                       (loop (case (random 4)
                               ((0) 'n)
                               ((1) 's)
                               ((2) 'e)
                               ((3) 'w))))
                      (else
                       (make-command 1 dir null)))))))))))))
                
  )