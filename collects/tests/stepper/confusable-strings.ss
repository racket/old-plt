(define (silly-choice str)
  (string-append str (if #f str str) str))

(silly-choice "family")