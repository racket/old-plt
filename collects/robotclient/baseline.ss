(module baseline mzscheme
  (require "board.ss"
	   "client-parameters.ss"
           (lib "list.ss"))
  (provide compute-baseline-move)
  
  (define (compute-drops)
    (filter (lambda (x) x)
            (map
             (lambda (p)
               (cond
                 ((and (= (get-player-x) (package-x p))
                       (= (get-player-y) (package-y p)))
		  p)
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
           (cons (car picks)
                 (loop (- remaining-weight (package-weight (car picks)))
                       (cdr picks))))
          (else (loop remaining-weight (cdr picks))))))
          
  (define (dist gx gy px py)
    (+ (abs (- px gx))
       (abs (- py gy))))
  
  (define (get-move-from-path path)
    (let* ((here (car path))
           (there (cadr path)))
      (cond
        ((= (car here) (car there))
         (cond
           ((> (cdr here) (cdr there)) 's)
           (else 'n)))
        (else
         (cond
           ((> (car here) (car there)) 'w)
           (else 'e))))))
     
  (define-struct queue (head tail))
  
  (define (create-queue)
    (make-queue null null))
  
  (define (enqueue! q v)
    (let ((new-cell (cons v null)))
      (cond
        ((not (null? (queue-tail q)))
         (set-cdr! (queue-tail q) new-cell)
         (set-queue-tail! q new-cell))
        (else (set-queue-head! q new-cell)
              (set-queue-tail! q new-cell)))))
  
  (define (dequeue! q)
    (begin0
      (car (queue-head q))
      (cond
        ((eq? (queue-head q) (queue-tail q))
         (set-queue-tail! q null)))
      (set-queue-head! q (cdr (queue-head q)))))
                  
  (define (compute-path px py goal?)
    (let ((visited (make-hash-table 'equal))
          (q (create-queue)))
      (enqueue! q (list (cons px py)))
      (let loop ()
        (let ((path (dequeue! q)))
          (cond
            ((goal? (car path)) (reverse! path))
            (else
             (for-each (lambda (spot)
                         (cond
                           ((not (hash-table-get visited spot (lambda () #f)))
                            (hash-table-put! visited spot #t)
                            (enqueue! q (cons spot path)))))
                       (filter (lambda (spot)
                                 (let ((t (get-type (get-spot (board) 
                                                              (car spot)
                                                              (cdr spot)))))
                                   (or (= t 0) (= t 3))))
                               (let ((x (caar path))
                                     (y (cdar path)))
                                 (list (cons (add1 x) y)
                                       (cons (sub1 x) y)
                                       (cons x (add1 y))
                                       (cons x (sub1 y))))))
             (loop)))))))
  
  (define path (make-parameter null))
  
  (define (compute-baseline-move packages robots)
    (let ((drops (compute-drops)))
      (cond
        ((not (null? drops))
	 (make-command 1 'd drops))
        (else
         (let ((picks (compute-picks packages)))
           (cond
	    ((not (null? picks))
	      (make-command 1 'p picks))
             (else
              (cond
                ((or (null? (path)) (null? (cdr (path))))
                 (path
                  (compute-path (get-player-x)
                                (get-player-y)
                                (let ((ph (packages-held)))
                                  (cond
                                    ((null? ph)
                                     (lambda (spot)
                                       (= 3 (get-type (get-spot (board)
                                                                (car spot)
                                                                (cdr spot))))))
                                    (else
                                     (lambda (spot)
                                       (ormap (lambda (p)
                                                (and (= (car spot) (package-x p))
                                                     (= (cdr spot) (package-y p))))
                                              ph)))))))))
              (begin0
                (make-command 1 (get-move-from-path (path)) null)
                (path (cdr (path)))))))))))
                
  )