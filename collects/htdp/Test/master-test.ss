;; TeachPack: master.ss
;; Language: Beginner

;; check-guess-two : sym sym sym sym -> sym
;; to determine whether targetI and guessI are the same
;; or whether at least some of guessI occur in targetI 
(define (check-guess-two target1 target2 guess1 guess2)
  (cond
    ((and (eq? target1 guess1) (eq? target2 guess2))
     'perfect_guess)
    ((or (eq? target1 guess1) (eq? target2 guess2))
     'one_color_at_correct_position)
    ((or (eq? target2 guess1) (eq? target1 guess2))
     'the_colors_occur)
    (else 'nothing_correct)))


(master check-guess-two)
