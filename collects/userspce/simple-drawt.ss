(require-library "simple-draw.ss" "userspce")

(start 100 100)

(draw-solid-line (make-posn 10 10) (make-posn 20 80))

(draw-solid-line (make-posn 20 80) (make-posn 100 20) BLUE)

(draw-solid-rect (make-posn 20 80) 10 10 RED)

(draw-solid-disk (make-posn 33 33) 10) 
