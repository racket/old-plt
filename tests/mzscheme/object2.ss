
; Test MzScheme's new object system

(load-relative "loadtest.ss")

(require (lib "class2.ss"))

(SECTION 'OBJECT2)

(define eater<%> (interface () eat))

(define fish%
  (class* object% (eater<%>)
    (public get-size grow eat)

    (init-field [size 1])

    (define (get-size) size)
    (define (grow s)
      (set! size (+ s size))
      size)
    (define (eat f)
      (grow (send f get-size)))

    (super-instantiate ())))

(define fish1 (make-object fish% 10))
(define fish2 (make-object fish% 100))

(test 10 'f1 (send fish1 get-size))
(test 100 'f2 (send fish2 get-size))

(test 12 'g1 (send fish1 grow 2))
(test 103 'g2 (send fish2 grow 3))

(test 115 'e (send fish2 eat fish1))

(define fish-size (class-field-accessor fish% 'size))
(test 12 fish-size fish1)
(test 115 fish-size fish2)

(define color-fish%
  (class fish%
    (public die)

    (init-field [color 'red])

    (define (die) (set! color 'black))

    (super-instantiate ())))

(define blue-fish (instantiate color-fish% () (color 'blue) (size 10)))
(define red-fish (instantiate color-fish% () (size 1)))

(define color-fish-color (class-field-accessor color-fish% 'color))

(test 'red color-fish-color red-fish)
(test 'blue color-fish-color blue-fish)

(test 1 'fr (send red-fish get-size))
(test 10 'fb (send blue-fish get-size))

(send red-fish grow 30)

(test 31 'fr (send red-fish get-size))

(test (void) 'fv (send blue-fish die))
(test 'black color-fish-color blue-fish)

(define picky-fish%
  (class fish%
    (override grow)
    (public set-limit)
    (rename [super-grow grow])

    (define pickiness 1)
    (define upper-limit 50)

    (define (grow s)
      (super-grow (min upper-limit (- s pickiness))))
    (define (set-limit v)
      (set! upper-limit v))

    (super-instantiate () (size 12))))

(define picky (make-object picky-fish%))

(test 12 'pf (send picky get-size))
(test 42 'pfe (send picky eat red-fish))
(test 42 'pf (send picky get-size))

(test (void) 'pfp (send picky set-limit 20))
(test 62 'pfe (send picky eat red-fish))

(test #t is-a? picky object%)
(test #t is-a? picky fish%)
(test #t is-a? picky picky-fish%)
(test #f is-a? picky color-fish%)

(test #t is-a? red-fish object%)
(test #t is-a? red-fish fish%)
(test #f is-a? red-fish picky-fish%)
(test #t is-a? red-fish color-fish%)

(test #t is-a? fish1 eater<%>)
(test #t is-a? picky eater<%>)
(test #t is-a? red-fish eater<%>)

(test #f is-a? 5 picky-fish%)
(test #f is-a? 5 eater<%>)

(test #t is-a? picky (class->interface picky-fish%))
(test #f is-a? red-fish (class->interface picky-fish%))

(err/rt-test (instantiate fish% () (bad-size 10)) exn:object?)
(err/rt-test (instantiate fish% () (size 10) (size 12)) exn:object?)
(err/rt-test (instantiate fish% (10) (size 12)) exn:object?)
(err/rt-test (instantiate picky-fish% () (size 17)) exn:object?)

(err/rt-test (color-fish-color picky))
(err/rt-test (color-fish-color 6))


(report-errs)
