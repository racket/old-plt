;; TeachPack : arrow.ss
;; Language: Beginner

(define RAD 10)

(define (translate sh delta)
  (make-posn (+ (posn-x sh) delta) (posn-y sh)))

(define (move sh delta)
  (cond
    [(and (clear-solid-disk sh RAD) (draw-solid-disk (translate sh delta) RAD))
     (translate sh delta)]
    [else false]))

(start 100 100)

(control-left-right (make-posn 10 20) 10 move)
