(require-library "error.ss" "htdp")
(require-library "draw.ss" "htdp")

(define-signature hangmanS (hangman hangman-list-repl))

(define hangmanU 
  (unit/sig hangmanS (import errorS plt:userspace^)

    #| ------------------------------------------------------------------------
    The Basic Constants |#

    (define TITLE "Hangman")
    (define WELCOME "Welcome to Hangman") 
    (define WINNER  "We have a winner!") 
    (define LOSER   "This is the end, my friend. The word was ~a.") 

    (define SMALL_A (char->integer #\a))
    (define LETTERS
      (build-list 26 (lambda (i) (format "~a" (integer->char (+ SMALL_A i))))))

    (define TRUMPET
      (make-object bitmap% 
	(build-path (collection-path "icons") "trumpet.xbm")
	'xbm))

    ;; char->symbol : char -> symbol 
    (define (char->symbol c)
      (string->symbol (format "~a" c)))

    #| ------------------------------------------------------------------------
    The GUI Layout: (computed as a function of constants)
   
    ------------------------------------------------------------------
    |       
    |   Choice   Check    StatusMessage
    | 
    | 
    |         ONE-MESSAGE
    ------------------------------------------------------------------

    Two horizontal panels: 
    the first one with all the colors (as buttons)
    the second is a sequence of colored buttons 
    |#

    (define frame (make-object frame% TITLE #f 100 50))
    (define verti (make-object vertical-panel% frame))

    (define panel (make-object horizontal-panel% verti))
    (send panel set-alignment 'center 'center)

    (define choice (make-object choice% "Guess:" LETTERS panel void))    

    (make-object message% "  " panel);; added for looks
    (define check (make-object button% "Check" panel 
		    (lambda (x y)
		      (check-guess
			(char->symbol
			  (list-ref LETTERS
			    (send choice get-selection)))))))

    (make-object message% " Status: " panel)
    (define status-message (make-object message% "___" panel))

    (define message-panel (make-object horizontal-panel% verti))
    (send message-panel set-alignment 'center 'center)
    (make-object message% WELCOME message-panel)

    #| ------------------------------------------------------------------------
    The functions for administrating the GUI |#
    
    ;; a-winner! : -> void
    ;; effect: signal win and disable game 
    (define (a-winner!)
      (set! check-guess void)
      (send message-panel change-children (lambda (x) null))
      (make-object message% WINNER message-panel)
      (make-object message% TRUMPET message-panel))

    ;; a-loser! : -> void
    ;; effect: signal loss and disable game 
    (define (a-loser!)
      (set! check-guess void)
      (send message-panel change-children (lambda (x) null))
      (make-object message% (format LOSER (uncover chosen)) message-panel))

    ;; check-guess : symbol -> word 
    ;; to check whether guess occurs in the chosen word, using reveal 
    ;; effect: update the status word 
    (define (check-guess guess)
      (let ((result (reveal chosen status guess)))
        (cond
          [(equal? result chosen) 
           (send status-message set-label (uncover chosen))
           (a-winner!)]
          [(equal? result status) 
           (draw-next-part (select-piece!))
           (when (the-end?) (a-loser!))]
          [else
	    (set! status result)
	    (send status-message set-label (uncover status))])))
    
    ;; uncover : word -> string
    ;; to translate the current word into a string, 
    ;; using abstraction breaking struct-ref
    (define (uncover a-word)
      (error 'hangman "impossible"))

    ;; State of Game: 
    (define PARTS 
      (vector 'right-leg 'left-leg 'left-arm 'right-arm 'body 'head 'noose))

    (define NUMBER-OF-GUESSES (vector-length PARTS))

    ;; pieces-left : index into PARTS 
    (define pieces-left NUMBER-OF-GUESSES)

    ;; select-piece! : -> void
    ;; effect: to decrease pieces-left and to pick the next thing to be drawn 
    (define (select-piece!)
      (when (= pieces-left 0)
        (error 'hangman-repl "can't happen")
        (send frame show #f))
      (set! pieces-left (sub1 pieces-left))
      (vector-ref PARTS pieces-left))
    ;; the-end? : -> boolean
    ;; to check whether the hangman is complet 
    (define (the-end?) (zero? pieces-left))

    #| User Interface: |#

    ;; chosen : word 
    (define chosen 10)
    ;; status : word 
    (define status 10)
    ;; reveal :  (word word letter -> word)
    (define (reveal chosen status guess)
      (error 'hangman-repl "appply hangman-repl first!"))
    ;; draw-next-part :  (symbol -> #t)
    (define (draw-next-part s)
      (error 'hangman-repl "appply hangman-repl first!"))

    ;; WORDS : (listof (list letter letter letter))
    (define WORDS
      (map (lambda (sym)
	     (map char->symbol (string->list (symbol->string sym))))
	'(and
	   are
	   but
	   cat
	   cow
	   dog
	   eat
	   fee
	   gal
	   hat
	   inn
	   kit
	   lit
	   met
	   now
	   owl
	   pet
	   rat
	   sea
	   the
	   usa
	   vip
	   was
	   zoo)))

    ;; hangman-repl :
    ;;   (letter letter letter -> word)
    ;;   (word word letter -> word)
    ;;   (symbol -> #t)
    ;;   ->
    ;;   void
    ;; effects: set up game status, draw noose, show frame
    ;; depends on: words are structures 
    (define (hangman mw rv dr)
      (check-proc 'hangman mw 3 '1st "3 arguments")
      (check-proc 'hangman rv 3 '2nd "3 arguments")
      (check-proc 'hangman dr 1 '3rd "1 argument")
      (set! chosen
	(apply mw (list-ref WORDS (random (length WORDS)))))
      (set! status (mw '_ '_ '_))
      (set! reveal rv)
      (set! draw-next-part dr)
      ;; make uncover work for structs
      (set! uncover
	(lambda (a-word)
	  (format "~a~a~a" 
	    (struct-ref a-word 0)
	    (struct-ref a-word 1)
	    (struct-ref a-word 2))))
      (draw-next-part (select-piece!))
      (send frame show #t))

    ;; THE FOLLOWING REQUIRES CHANGE!

    ;; word2 = (listof letter)
    ;; hangman-list :
    ;;   word2 word2 (word2 word2 letter -> word2) (symbol -> #t) -> void
    ;; effects: set up game status, draw noose, show frame
    (define (hangman-list-repl ch st rv dr)
      (check-arg 'hangman-list-repl (list? ch) "proper list" '1st ch)
      (check-arg 'hangman-list-repl (list? st) "proper list" '2nd st)
      (unless (= (length ch) (length st))
	(error 'hangman-list-repl
	  "chosen word and status word must be of same length"))
      (check-proc 'hangman-list-repl rv 3 '3rd "3 arguments")
      (check-proc 'hangman-list-repl dr 1 '4th "1 argument")
      (set! chosen ch)
      (set! status st)
      (set! reveal rv)
      (set! draw-next-part dr)
      ;; make uncover work for lists
      (set! uncover
	(lambda (word)
	  (apply string-append (map (lambda (x) (format "~a" x)) word))))
      (draw-next-part (select-piece!))
      (send frame show #t))
    ))

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    [ERR   : errorS (errorU)]
    [DRAW  : drawS  (bigDrawU ERR PLT)]
    [HANG  : hangmanS (hangmanU ERR PLT)])
  (export (open DRAW) (open HANG)))
