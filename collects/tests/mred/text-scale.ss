
(require (lib "math.ss"))

(define measure-after? #f)
(define rotate? #f)
(define symbol? #t)
(define latin-1? #f)

(define normal-font (make-object font% 
				 14 
				 #;"-*-lucida" 
				 (if symbol? 'symbol 'default)
				 'normal 'normal))

(define no-brush (make-object brush% "white" 'transparent))
(define xor-pen (make-object pen% "black" 0 'xor))
(define yellow (make-object color% "yellow"))

(define str (format "This is a t~ast"
		    (if latin-1? "\351" "e")))

(define (draw-one dc sx sy y w h d)
  (send dc set-text-mode 'solid)
  (send dc set-text-background yellow)
  (send dc set-scale sx sy)
  (if rotate?
      (send dc draw-text str (/ 100 sx) (/ y sy) #f 0 (* pi 1/4))
      (send dc draw-text str (/ 100 sx) (/ y sy)))
  (if measure-after?
      (let-values ([(w h d a) (send dc get-text-extent str)])
	(send dc draw-rectangle (/ 100 sx) (/ y sy) w h))
      (send dc draw-rectangle (/ 100 sx) (/ y sy) w h))
  (send dc set-scale 1 1))

(define (draw-all dc)
  (send dc set-font normal-font)
  (send dc set-brush no-brush)
  (send dc set-pen xor-pen)
  (let-values ([(w h d a) (send dc get-text-extent str)])
    (draw-one dc 1 1 10 w h d)
    (draw-one dc 2 2 (+ 15 h) w h d)
    (draw-one dc 0.9 0.9 (+ 20 (* 3 h)) w h d)
    (draw-one dc 0.75 0.75 (+ 25 (* 4 h)) w h d)
    (draw-one dc 2 1 (+ 30 (* 5 h)) w h d)
    (draw-one dc 1 2 (+ 40 (* 6 h)) w h d)
    (draw-one dc 2.1 2.1 (+ 45 (* 8 h)) w h d)
    (draw-one dc 2.05 2.05 (+ 45 (* 10.2 h)) w h d)
    (draw-one dc 1.95 1.95 (+ 50 (* 12.2 h)) w h d)
    (draw-one dc 1.93 1.93 (+ 55 (* 14.2 h)) w h d)
    (draw-one dc 1.90 1.90 (+ 60 (* 16.2 h)) w h d)
    ))


(define f (new frame%
	       [label "Scale Test"]
	       [width 400]
	       [height 500]))
(define c (new canvas%
	       [parent f]
	       [paint-callback
		(lambda (c dc)
		  (send dc clear)
		  (draw-all dc))]))

(send f show #t)
