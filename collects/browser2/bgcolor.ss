(require-library "refer.ss")
(require-library "macro.ss")
(require-library "function.ss")

; A bgcolor-text% is a text% supporting a background color.
; bgcolor is a color% object representing the background color.
(define bgcolor-text%
  (class text% (bgcolor)
    (rename [super-on-paint on-paint])
    (inherit get-admin, invalidate-bitmap-cache)
    (sequence (super-init))
    (override 
      [on-paint
       (lambda (before? dc left top right bottom dx dy draw-caret)
         (super-on-paint before? dc left top right bottom dx dy draw-caret)
         (local [(define orig-pen (send dc get-pen))
                 (define orig-brush (send dc get-brush))
                 (define admin (get-admin))
                 (define (draw-rectangle-and-fill l t b r)
                   (if (and (> (- r l) 0)
                            (> (- b t) 0))
                       (send dc draw-rectangle l t (- r l) (- b t))
                       (void)))]
           (unless (boolean? admin)
             (send dc set-pen (send the-pen-list find-or-create-pen bgcolor 1 'solid))
             (send dc set-brush (send the-brush-list find-or-create-brush bgcolor 'solid))
             (if (boolean=? before? #t)
                 (draw-rectangle-and-fill (+ dx left) (+ dy top) (+ dy bottom) (+ dx right)))
             ;(send this invalidate-bitmap-cache 0 0 800 800)
             (send dc set-pen orig-pen)
             (send dc set-brush orig-brush))))])))

(define frame (make-object frame% "" #f 800 800))
(define bgcolor-text (make-object bgcolor-text% (make-object color% 100 100 100)))
(define editor-canvas (make-object editor-canvas% frame bgcolor-text))
(send frame show #t)
