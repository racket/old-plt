;; TeachPack: hangman.ss
;; Language: Beginner

;; A letter is 'a ... 'z, plus '_

(define-struct word (a b c))
;; A word is (make-word letter letter letter)

;; reveal : word word letter -> word 
;; to apply reveal1 to each position 
(define (reveal chosen status guess)
  (make-word (reveal1 (word-a chosen) (word-a status) guess)
             (reveal1 (word-b chosen) (word-b status) guess)
             (reveal1 (word-c chosen) (word-c status) guess)))

;; reveal1 : letter letter letter -> letter
;; to pick ch if ch = st, gu if ch = gu, and st otherwise 
(define (reveal1 ch st gu)
  (cond
    ((eq? ch gu) gu)
    ((eq? ch st) st)
    (else st)))


#| ------------------------------------------------------------------------
   draw-next-part :
    { 'noose 'head 'right-arm 'left-arm 'body 'right-leg 'left-leg } -> #t
   result: #t if things went okay
   effect: to draw the specified body part in a canvas of size W x H
   credit: John Clements 
|#
(define (draw-next-part body-part)
  (cond ((eq? body-part 'body)
         (draw-solid-line (make-posn 100 60) (make-posn 100 130) BLACK))
        ((eq? body-part 'right-leg)
         (draw-solid-line (make-posn 100 130) (make-posn 30 170) BLACK))
        ((eq? body-part 'left-leg)
         (draw-solid-line (make-posn 100 130) (make-posn 170 170) BLACK))
        ((eq? body-part 'right-arm)
         (draw-solid-line (make-posn 100 75) (make-posn 40 65) BLACK))
        ((eq? body-part 'left-arm)
         (draw-solid-line (make-posn 100 75) (make-posn 160 65) BLACK))
        ((eq? body-part 'head)
         (draw-circle (make-posn 100 50) 10 BLACK))
        ((eq? body-part 'noose)
	 (and
	  (draw-solid-line (make-posn 100 30) (make-posn 100 10) BLACK)
	  (draw-solid-line (make-posn 100 10) (make-posn 0 10) BLACK)
	  (draw-solid-line (make-posn 115 35) (make-posn 123 43) BLACK)
	  (draw-solid-line (make-posn 123 35) (make-posn 115 43) BLACK)
	  (draw-solid-line (make-posn 131 40) (make-posn 139 48) BLACK)
	  (draw-solid-line (make-posn 139 40) (make-posn 131 48) BLACK)
	  (draw-circle (make-posn 120 50) 30 RED)))))

;; reveal-list : list-of-letters list-of-letters letter -> list-of-letters
;; to apply reveal1 to each position 
(define (reveal-list word1 word2 letter)
  (cond
    ((empty? word1) empty)
    (else (cons (reveal1 (first word1) (first word2) letter)
                (reveal-list (rest word1) (rest word2) letter)))))

(define (reveal-list l1 l2 gu)
  (map (lambda (x1 x2)
         (cond
           [(eq? x1 gu) gu]
           [else x2]))
       l1 l2))

;; TESTS: 

; (equal? (reveal-list (list 'd 'e 'r) (list '_ '_ '_) 'd) (list 'd'_ '_))
; (equal? (reveal-list (list 'd 'e 'r) (list '_ '_ '_) 'f) (list '_ '_ '_))

;; test error checking
;(hangman-repl (list 'd 'e 'r) (make-word '_ '_ '_) reveal-list draw-next-part)

(start 200 200)
(hangman make-word reveal draw-next-part)
; (hangman-list reveal-list draw-next-part)
