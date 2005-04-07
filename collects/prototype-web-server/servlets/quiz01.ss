(module quiz01 (lib "persistent-web-interaction.ss" "prototype-web-server")
  (require "quiz-lib.ss"
           (lib "url.ss" "net")
           (lib "servlet-helpers.ss" "web-server"))
  
  ;; get-answer: mc-question -> number
  ;; get an answer for a multiple choice question
  (define (get-answer mc-q)
    (let* ([req
           (send/suspend/hidden (make-cue-page mc-q))]
           [bdgs (request-bindings req)])
      (if (exists-binding? 'answs bdgs)
          (string->number
           (extract-binding/single
            'answs bdgs))
          -1)))
  
  ;; get-answers: (listof mc-question) -> (listof number)
  ;; get answers for all of the quiz questions.
  (define (get-answers mc-qs)
    (cond
      [(null? mc-qs) '()]
      [else
       (cons
        (get-answer (car mc-qs))
        (get-answers (cdr mc-qs)))]))
  
  ;; tally-results: (listof mc-question) (listof number) -> number
  ;; count the number of correct answers
  (define (tally-results mc-qs answs)
    (cond
      [(null? mc-qs) 0]
      [(= (car answs)
          (mc-question-correct-answer (car mc-qs)))
       (add1 (tally-results (cdr mc-qs) (cdr answs)))]
      [else (tally-results (cdr mc-qs) (cdr answs))]))
  
  (let ([initial-request (start-servlet)])
    `(html (head (title "Final Page"))
           (body
            (h1 "Quiz Results")
            (p ,(format "You got ~a correct out of ~a questions."
                        (tally-results quiz (get-answers quiz))
                        (length quiz)))
            (p "Thank you for taking the quiz"))))
  )