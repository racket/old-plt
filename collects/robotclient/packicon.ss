(module packicon mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (provide make-package-colors
           make-package-icons
           make-robot-colors
           make-robot-icons)
  
  (define (rel-color rel-weight)
    (make-object color% 
      (inexact->exact (floor (* rel-weight 255)))
      0
      (inexact->exact (floor (* (- 1.0 rel-weight) 255)))))

  (define (mk-rel-weights num-bins)
    (let ([n (sub1 num-bins)])
      (let loop ([i 0])
        (if (= i n)
            '(1)
            (cons (/ i n) (loop (add1 i)))))))
  
  (define (make-package-colors num-bins)
    (map rel-color (mk-rel-weights num-bins)))
  
  (define (mk-package color size)
    (define pack-bm (make-object bitmap% size size))
    (define pack-mask (make-object bitmap% size size #t))
    (define dc (make-object bitmap-dc% pack-bm))

    (define top (list (make-object point% (* size 1/2) (* size 1/4))
                      (make-object point% (* size 3/4) (* size 3/8))
                      (make-object point% (* size 1/2) (* size 1/2))
                      (make-object point% (* size 1/4) (* size 3/8))))
    (define right (list (make-object point% (* size 3/4) (* size 3/8))
                        (make-object point% (* size 1/2) (* size 1/2))
                        (make-object point% (* size 1/2) (* size 3/4))
                        (make-object point% (* size 3/4) (* size 5/8))))
    (define left (list (make-object point% (* size 1/4) (* size 3/8))
                       (make-object point% (* size 1/2) (* size 1/2))
                       (make-object point% (* size 1/2) (* size 3/4))
                       (make-object point% (* size 1/4) (* size 5/8))))
    
    (define dx (* size 1/5))
    (define dy (* size 1/5))
    
    (send dc clear)
    (send dc set-brush (make-object brush% color 'solid))
    (send dc draw-polygon top dx dy)
    (send dc draw-polygon left dx dy)
    (send dc draw-polygon right dx dy)
    
    (send dc set-bitmap pack-mask)
    (send dc clear)
    (send dc draw-polygon top dx dy)
    (send dc draw-polygon left dx dy)
    (send dc draw-polygon right dx dy)
    
    (send dc set-bitmap #f)
    (send pack-bm set-loaded-mask pack-mask)
    
    pack-bm)
  
  (define (make-package-icons size colors)
    (map (lambda (color)
           (mk-package color size))
         colors))

  (define (make-robot-colors n)
    (let loop ([r 255][g 0][b 0][rr 255][gg 255][bb 0][rrr 255][ggg 128][bbb 128][i 0])
      (if (= i n)
          null
          (cons
           (make-object color% r g b)
           (if (zero? (modulo (add1 i) 3))
               (if (zero? (modulo (add1 i) 9))
                   (loop rr gg bb rrr ggg bbb (quotient g 2) g r (add1 i))
                   (loop rr gg bb rrr ggg bbb (quotient g 2) b r (add1 i)))
               (loop g b r rr gg bb rrr ggg bbb (add1 i)))))))

  (define pi (atan 0 -1))
  
  (define (mk-robot color size)
    (define robot-bm (make-object bitmap% size size))
    (define robot-mask (make-object bitmap% size size #t))
    (define dc (make-object bitmap-dc% robot-bm))
    (define (draw-once)
      (send dc draw-rectangle 
            (* 1/10 size) (* 8/30 size)
            (* 1/2 size) (* 11/20 size))
      (send dc draw-arc 
            (* 1/10 size) (* 3/10 size)
            (* 1/2 size) (* 1/3 size)
            pi (* 2 pi))
      (send dc draw-ellipse 
            (* 1/10 size) (* 1/10 size)
            (* 1/2 size) (* 1/3 size))
      (send dc draw-arc 
            (* 1/10 size) (* 6/10 size)
            (* 1/2 size) (* 1/3 size)
            pi (* 2 pi)))

    (send dc clear)
    (send dc set-brush (make-object brush% color 'solid))
    (draw-once)
    
    (send dc set-bitmap robot-mask)
    (send dc clear)
    (draw-once)
    
    (send dc set-bitmap #f)
    (send robot-bm set-loaded-mask robot-mask)
    
    robot-bm)

  (define (make-robot-icons size colors)
    (map (lambda (color)
           (mk-robot color size))
         colors)))
