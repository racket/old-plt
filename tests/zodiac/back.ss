(unless (defined? 'SECTION)
  (load-relative (build-path 'up "mzscheme" "testing.ss")))

(SECTION 'back-boxes)

(unless (defined? 'zodiac:see-parsed) ; chosen fairly arbitrarily
  (require-library "invoke.ss" "zodiac"))

(define (set-read-test parsed getter setter old new)
  (test old getter (zodiac:parsed-back parsed))
  (test (void) setter (zodiac:parsed-back parsed) new)
  (test new getter (zodiac:parsed-back parsed)))

(define read
  ((zodiac:read (open-input-string "3")
                (zodiac:make-location 1 1 0 "string-input"))))

(define parsed-1 (zodiac:scheme-expand read))

(define-values (getter setter) 
  (zodiac:register-client 'first-client (lambda () 13)))

(set-read-test parsed-1 getter setter 13 "a string")

(define parsed-2 (zodiac:scheme-expand read))

(set-read-test parsed-2 getter setter 13 #t)

(define-values (getter-2 setter-2) (zodiac:register-client 'second-client (lambda () 15)))
(define-values (getter-3 setter-3) (zodiac:register-client 'third-client (lambda () 16)))

(set-read-test parsed-1 getter-3 setter-3 16 'a-symbol)
(test "a string" getter (zodiac:parsed-back parsed-1))

(define-values (getter-4 setter-4) (zodiac:register-client 'bad-client 
                                                           (lambda () (error "ouch"))))

(set-read-test parsed-1 getter-2 setter-2 15 'matador)

(error-test '(getter-4 (zodiac:parsed-back parsed-1)) exn:user?)
(error-test '(setter-4 (zodiac:parsed-back parsed-2) 'okay-value) exn:user?)

(report-errs)

