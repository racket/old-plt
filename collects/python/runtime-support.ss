(module runtime-support mzscheme
  (require (lib "list.ss")
           "primitives.ss")
  (provide (all-defined))
  
  ;;;;;;;;;; Python Runtime Support by Daniel ;;;;;;;;;
  
  ;; ==: X X -> bool
  (define(== a b)
    (cond
      [(number? a) (= a b)]
      [(string? a) (string=? a b)]
      [(symbol? a) (eq? a b)]
;      [(tuple? a) (andmap == (tuple-list a) (tuple-list b))]
      [(list? a) (andmap == a b)]
      [else (error (format "No runtime support to compare ~a and ~a yet" a b))]))
  
  ;; py-print: (listof X) -> void
  (define (py-print lst)
    (for-each (lambda (x)
                (display (py-string%->string (if (py-is-a? x py-string%)
                                                 x
                                                 (py-call py-repr (list x))))) (display #\space))
              lst)
    (newline))

  )