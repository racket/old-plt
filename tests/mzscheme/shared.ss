;; this writes values to strings and compares the strings
;; to implements an equal? predicate that works for cyclic
;; structures. When/if equal? works for cyclic stuff, this
;; should be changed to equal? directly.

(unless (defined? 'SECTION)
  (load-relative "testing.ss"))

(SECTION 'shared)

(require-library "shared.ss")

(let ()
  (define (gs v)
    (let ([p (open-output-string)])
      (write v p)
      (get-output-string p)))
  (define (stest expect expression)
    (test
     (gs expect)
     (let ([shared-tester (lambda (x) (gs (eval x)))]) shared-tester)
     expression))
  
  (stest '(1 2) '(shared ([x (list 1 2)]) x))
  (stest #(1 2) '(shared ([x (vector 1 2)]) x))
  (stest (box 1) '(shared ([x (box 1)]) x))
  (stest '(1 . 2) '(shared ([x (cons 1 2)]) x))
  
  (stest '(1 2) '(shared ([x (#%list 1 2)]) x))
  (stest #(1 2) '(shared ([x (#%vector 1 2)]) x))
  (stest (box 1) '(shared ([x (#%box 1)]) x))
  (stest '(1 . 2) '(shared ([x (#%cons 1 2)]) x))

  (stest '#1=(#1# 1) '(shared ([x (list x 1)]) x))
  (stest '#2=#(#2# 1) '(shared ([x (vector x 1)]) x))
  (stest '#3=#&#3# '(shared ([x (box x)]) x))
  (stest '#4=(#4# . 1) '(shared ([x (cons x 1)]) x))
  (stest '#5=(1 . #5#) '(shared ([x (cons 1 x)]) x))

  (stest '#6=(#6# 1) '(shared ([x (#%list x 1)]) x))
  (stest '#7=#(#7# 1) '(shared ([x (#%vector x 1)]) x))
  (stest '#8=#&#8# '(shared ([x (#%box x)]) x))
  (stest '#9=(#9# . 1) '(shared ([x (#%cons x 1)]) x))
  (stest '#10=(1 . #10#) '(shared ([x (#%cons 1 x)]) x))
  
  (stest '#11=(#11#) '(shared ([x `(,x)]) x)))
  
(report-errs)
