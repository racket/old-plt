
(require (lib "math.ss"))

(define measure-after? #f)
(define rotate? #f)
(define symbol? #f)
(define latin-1? #f)
(define lucida? #f)
(define change-font? #f)

(define no-brush (make-object brush% "white" 'transparent))
(define xor-pen (make-object pen% "black" 0 'xor))
(define yellow (make-object color% "yellow"))

(define (get-the-font size)
  (apply
   make-object font% 
   size
   (append
    (if lucida?
	'("-*-lucida")
	'())
    (list
     (if symbol? 'symbol 'default)
     'normal 'normal))))

(define (draw-one dc str sx sy y w h d)
  (define csx 1)
  (define csy 1)
  (send dc set-text-mode 'solid)
  (send dc set-text-background yellow)
  (if change-font?
      (begin
	(send dc set-font (get-the-font (inexact->exact (floor (* sy 12)))))
	(set! csx sx)
	(set! csy sy)
	(set! sx 1)
	(set! sy 1))
      (send dc set-scale sx sy))
  (if rotate?
      (send dc draw-text str (/ 100 sx) (/ y sy) #f 0 (* pi 1/4))
      (send dc draw-text str (/ 100 sx) (/ y sy)))
  (if measure-after?
      (let-values ([(w h d a) (send dc get-text-extent str)])
	(send dc draw-rectangle (/ 100 sx) (/ y sy) w h))
      (send dc draw-rectangle (/ 100 sx) (/ y sy) (* w csx) (* h csy)))
  (send dc set-scale 1 1))

(define (draw-all dc)
  (define normal-font (get-the-font 12))
  (define str (format "This is a t~ast"
		      (if latin-1? "\351" "e")))
  (send dc set-font normal-font)
  (send dc set-brush no-brush)
  (send dc set-pen xor-pen)
  (let-values ([(w h d a) (send dc get-text-extent str)])
    (draw-one dc str 1 1 10 w h d)
    (draw-one dc str 2 2 (+ 15 h) w h d)
    (draw-one dc str 0.9 0.9 (+ 20 (* 3 h)) w h d)
    (draw-one dc str 0.75 0.75 (+ 25 (* 4 h)) w h d)
    (draw-one dc str 2 1 (+ 30 (* 5 h)) w h d)
    (draw-one dc str 1 2 (+ 40 (* 6 h)) w h d)
    (draw-one dc str 2.1 2.1 (+ 45 (* 8 h)) w h d)
    (draw-one dc str 2.05 2.05 (+ 45 (* 10.2 h)) w h d)
    (draw-one dc str 1.95 1.95 (+ 50 (* 12.2 h)) w h d)
    (draw-one dc str 1.93 1.93 (+ 55 (* 14.2 h)) w h d)
    (draw-one dc str 1.90 1.90 (+ 60 (* 16.2 h)) w h d)
    ))

(define f (new frame%
	       [label "Scale Test"]
	       [width 400]
	       [height 500]))

(define pane1 (new horizontal-pane% 
		   [parent f]
		   [stretchable-height #f]))
(define pane2 (new horizontal-pane% 
		   [parent f]
		   [stretchable-height #f]))

(define-syntax make-checkbox
  (syntax-rules ()
    [(_ who pane)
     (new check-box% 
	  [label (symbol->string 'who)]
	  [parent pane]
	  [callback (lambda (cb e)
		      (set! who (send cb get-value))
		      (send c refresh))])]))

(make-checkbox measure-after? pane1)
(make-checkbox change-font? pane1)
(make-checkbox rotate? pane1)
(make-checkbox symbol? pane2)
(make-checkbox latin-1? pane2)
(make-checkbox lucida? pane2)

(define c (new canvas%
	       [parent f]
	       [paint-callback
		(lambda (c dc)
		  (send dc clear)
		  (draw-all dc))]))

(send f show #t)
