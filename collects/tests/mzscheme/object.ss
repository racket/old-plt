
; Test MzScheme's new object system

(load-relative "loadtest.ss")

(require (lib "class.ss"))

(SECTION 'OBJECT)

;; ------------------------------------------------------------
;; Test syntax errors

(syntax-test #'class)
(syntax-test #'(class))
(syntax-test #'(class . object%))
(test #t class? (class object%))
(syntax-test #'(class object% . 10))

(define (test-init/field init)
  (teval #`(test #t class? (class object% (#,init))))
  (syntax-test #`(class object% (#,init . x)))
  (syntax-test #`(class object% (#,init 10)))
  (syntax-test #`(class object% (#,init (x . 10))))
  (syntax-test #`(class object% (#,init (x 10 10))))
  (syntax-test #`(class object% (#,init (x 10 . 10))))
  (syntax-test #`(class object% (#,init (10 10))))
  (teval #`(test #t class? (class object% (#,init (x 10)))))
  (syntax-test #`(class object% (#,init ((x) 10))))
  (syntax-test #`(class object% (#,init ((x . y) 10))))
  (syntax-test #`(class object% (#,init ((x y . z) 10))))
  (syntax-test #`(class object% (#,init ((x y z) 10))))
  (syntax-test #`(class object% (#,init ((x 10) 10))))
  (syntax-test #`(class object% (#,init ((10 x) 10))))
  (teval #`(test #t class? (class object% (#,init ((x y) 10)))))
  (syntax-test #`(class object% (#,init ((x y) 10) . z)))
  (syntax-test #`(class object% (#,init ((x y) 10) z)))
  (syntax-test #`(class object% (#,init ((x y) 10) (z))))
  (teval #`(test #t class? (class object% (#,init ((x y) 10) (z 5)))))
  (syntax-test #`(class object% (#,init (x 10) y)))
  (syntax-test #`(class object% (#,init (x 10)) (#,init y)))

  (syntax-test #`(class object% (#,init x x)))
  (syntax-test #`(class object% (#,init x) (#,init x)))
  (syntax-test #`(class object% (#,init x) (#,init (x 10))))
  (syntax-test #`(class object% (#,init (x 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((y x) 10)) (#,init ((z x) 10))))
  (syntax-test #`(class object% (#,init x) (#,init x)))
  (syntax-test #`(class object% (#,init x) (#,init (x 10))))
  (syntax-test #`(class object% (#,init (x 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init (x 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((x y) 10)) (#,init ((x z) 10))))
  (syntax-test #`(class object% (#,init ((y x) 10)) (#,init ((z x) 10))))

  (teval #`(test #t class? (class object% (#,init ((x x) 10)))))
  (teval #`(test #t class? (class object% (#,init ((x x) 10) ((y y) 10)))))
  (teval #`(test #t class? (class object% (#,init ((x x) 10)) (#,init ((y y) 10)))))
  (teval #`(test #t class? (class object% (#,init ((x y) 10)) (#,init ((y x) 10)))))

  'ok)

(define (test-init init)
  (teval #`(test #t class? (class object% (#,init x))))
  (teval #`(test #t class? (class object% (#,init ((x y))))))
  (test-init/field init)

  (syntax-test #`(class object% (init-rest) (#,init y)))
  (syntax-test #`(class object% (#,init x) (init-rest) (#,init y)))
  (syntax-test #`(class object% (#,init y) (init-rest y)))
  (teval #`(test #t class? (class object% (#,init [(x y)]) (init-rest y))))

  'ok)

(test-init #'init)
(test-init #'init-field)
(test-init/field #'field)

(syntax-test #'(class object% (init-rest 10)))
(syntax-test #'(class object% (init-rest . x)))
(syntax-test #'(class object% (init-rest x y)))
(syntax-test #'(class object% (init-rest) (init-rest x)))
(syntax-test #'(class object% (init-rest x) (init-rest)))

(syntax-test #'(class object% (init-field (x 10)) (init y)))
(syntax-test #'(class object% (init (x 10)) (init-field y)))
(syntax-test #'(class object% (init-rest x) (init y)))
(syntax-test #'(class object% (init-rest x) (init-field y)))

(define to-override-class%
  (class object%
    (public x y)
    (define (x) 1)
    (define (y) 2)))

(define (test-method public object%)
  (teval #`(test #t class? (class #,object% (#,public))))
  (syntax-test #`(class #,object% (#,public . x)))
  (syntax-test #`(class #,object% (#,public 10)))
  (syntax-test #`(class #,object% (#,public (x))))
  (syntax-test #`(class #,object% (#,public (x . y))))
  (syntax-test #`(class #,object% (#,public (x 10))))
  (syntax-test #`(class #,object% (#,public (10 x))))
  (syntax-test #`(class #,object% (#,public x . y)))
  (syntax-test #`(class #,object% (#,public x 10)))

  (syntax-test #`(class #,object% (#,public x x)))
  (syntax-test #`(class #,object% (#,public x) (#,public x)))
  (syntax-test #`(class #,object% (#,public (x y) (x y))))
  (syntax-test #`(class #,object% (#,public (x y1) (x y))))
  (syntax-test #`(class #,object% (#,public (x y) (x2 y))))
  (unless (module-identifier=? public #'private)
    (teval #`(test #t class? (class #,object% (#,public (x x)) (define (x) 1))))
    (teval #`(test #t class? (class #,object% (#,public (x y) (y x)) (define (x) 1) (define (y) 2)))))

  'ok)

(test-method #'public #'object%)
(test-method #'public-final #'object%)
(test-method #'override #'to-override-class%)
(test-method #'override-final #'to-override-class%)
(test-method #'private #'object%)

(syntax-test #'(class*/names foo object% ()))
(syntax-test #'(class*/names (1) object% ()))
(syntax-test #'(class*/names (ths 1) object% ()))
(syntax-test #'(class*/names (ths sup-i 1) object% ()))
(syntax-test #'(class*/names (ths sup-i sup-mo 1) object% ()))
(syntax-test #'(class*/names (ths sup-i sup-mo sup-n 10) object% ()))
(syntax-test #'(class*/names (ths sup-i sup-mo sup-n . arg) object% ()))

;; ------------------------------------------------------------
;; Test badic functionality

(define eater<%> (interface () eat))

(define-syntax mk-noop
  (syntax-rules 
   ()
   [(_ name)
    (begin
      (define (name) (blah))
      (define (blah)
	(printf "hi~n")))]))

(define fish%
  (class* object% (eater<%>)
    (public get-size grow eat)
    (public-final noop)

    (mk-noop noop)
    (private increase-size eat-sized-fish)

    (init-field [size 1])

    ;; Private methods
    (define (increase-size s)
      (set! size (+ s size)))
    (define (eat-sized-fish s)
      (grow s))

    ;; Public methods
    (define (get-size) size)
    (define (grow s)
      (noop)
      (set! size (+ s size))
      size)
    (define (eat f)
      (let ([this 5]) ; <- make sure methods still work...
	(grow (send f get-size))))

    (super-instantiate ())))

(define fish1 (make-object fish% 10))
(define fish2 (make-object fish% 100))

(test 10 'f1 (send fish1 get-size))
(test 100 'f2 (send fish2 get-size))

(test 12 'g1 (send fish1 grow 2))
(test 103 'g2 (send fish2 grow 3))

(test 115 'e (send fish2 eat fish1))

(define fish-size (class-field-accessor fish% size))
(test 12 fish-size fish1)
(test 115 fish-size fish2)

(define color-fish%
  (class fish%
    (public-final die)
    (inherit get-size)
    (inherit-field size)

    (init-field [color 'red])

    (define (die)
      (unless (= size (get-size))
	(error 'bad))
      (set! color 'black))

    (super-new)))

(define blue-fish (instantiate color-fish% () (color 'blue) (size 10)))
(define red-fish (instantiate color-fish% () (size 1)))

(define color-fish-color (class-field-accessor color-fish% color))

(test 'red color-fish-color red-fish)
(test 'blue color-fish-color blue-fish)

(test 1 'fr (send red-fish get-size))
(test 10 'fb (send blue-fish get-size))

(send red-fish grow 30)

(test 31 'fr (send red-fish get-size))

(test (void) 'fv (send blue-fish die))
(test 'black color-fish-color blue-fish)

(let ([exn (with-handlers ([not-break-exn? (lambda (exn) exn)])
	     (send red-fish get-size 10))])
  (test #t exn:application:arity? exn)
  (test 1 exn:application-value exn)
  (test 0 exn:application:arity-expected exn))

(define picky-fish%
  (class fish%
    (override grow)
    (public set-limit)
    (rename [super-grow grow])

    (define pickiness 1)
    (define upper-limit 50)

    (define grow
      ;; Test method-declaration shape with variable:
      (let ([grow (lambda (s)
		    (super-grow (min upper-limit (- s pickiness))))])
	grow))
    (define set-limit 
      ;; Test method-declaration shape with body method:
      (let* ([check-pickiness (lambda (p)
				(unless (= p pickiness)
				  (error 'ack)))]
	     [set-upper (lambda (v p)
			  (check-pickiness p)
			  (set! upper-limit v))])
	(lambda (v)
	  (set-upper v pickiness))))

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

(err/rt-test (class color-fish% (override die) (define die (lambda () 'x))) exn:object?)

(define rest-arg-fish%
  (class fish%
    (public greeting)

    (begin ; should get flattened
      (init -first-name)
      (init-field last-name)
      (init-rest -nicknames))

    (define first-name -first-name) 
    (define nicknames -nicknames) 

    (define greeting
      (letrec ([loop
		(case-lambda
		 [() (loop first-name last-name)]
		 [(last-name first-name) ;; intentionally backwards to test scope
		  (format "~a ~a, a.k.a.: ~a"
			  last-name first-name
			  nicknames)]
		 [(a b c d . rest) 'never-get-here])])
	loop))

    (define/public useless-method (case-lambda))

    (super-new (size 12))))

(define rest-fish (make-object rest-arg-fish% "Gil" "Finn" "Slick"))

(test "Gil Finn, a.k.a.: (Slick)" 'osf (send rest-fish greeting))

(let ([exn (with-handlers ([not-break-exn? (lambda (exn) exn)])
	     (send rest-fish greeting 1 2 3))])
  (test #t exn:application:arity? exn)
  (test 3 exn:application-value exn)
  (test (list 0 2 (make-arity-at-least 4)) exn:application:arity-expected exn))
(let ([exn (with-handlers ([not-break-exn? (lambda (exn) exn)])
	     (send rest-fish useless-method 3))])
  (test #t exn:application:arity? exn)
  (test 1 exn:application-value exn)
  (test null exn:application:arity-expected exn))

;; Missing last-name:
(err/rt-test (instantiate rest-arg-fish% () (-first-name "Gil") (-nicknames null)) 
	     exn:object?)

(define rest-fish-0 (instantiate rest-arg-fish% () (-first-name "Gil") (last-name "Finn")))
(test "Gil Finn, a.k.a.: ()" 'osf (send rest-fish-0 greeting))

;; Keyword order doesn't matter:
(define rest-fish-0.5 (instantiate rest-arg-fish% () (last-name "Finn") (-first-name "Gil")))
(test "Gil Finn, a.k.a.: ()" 'osf (send rest-fish-0.5 greeting))

(err/rt-test (instantiate rest-arg-fish% () 
			  (-first-name "Gil") (last-name "Finn") 
			  (-nicknames "Slick"))
	     exn:object?)
(err/rt-test (instantiate rest-arg-fish% () 
			  (-first-name "Gil") (last-name "Finn") 
			  (anything "Slick"))
	     exn:object?)

;; Redundant by-pos:
(err/rt-test (instantiate rest-arg-fish% ("Gil") (-first-name "Gilly") (last-name "Finn"))
	     exn:object?)

(define no-rest-fish%
  (class fish%
    (public greeting)

    (init-field first-name)
    (init-field last-name)
    (init-rest)

    (define (greeting)
      (format "~a ~a" last-name first-name))

    (super-instantiate (12))))

;; Too many by-pos:
(err/rt-test (instantiate no-rest-fish% ("Gil" "Finn" "hi" "there"))
	     exn:object?)

(define no-rest-0 (instantiate no-rest-fish% ("Gil" "Finn")))
(test 12 'norest (send no-rest-0 get-size))

(define allow-rest-fish%
  (class fish%
    (public greeting)

    (init-field first-name)
    (init-field last-name)

    (define (greeting)
      (format "~a ~a" last-name first-name))

    (super-instantiate ())))

;; Too many by-pos:
(err/rt-test (instantiate no-rest-fish% ("Gil" "Finn" 18 20))
	     exn:object?)

(define no-rest-0 (instantiate allow-rest-fish% ("Gil" "Finn" 18)))
(test 18 'allowrest (send no-rest-0 get-size))


(define allow-rest/size-already-fish%
  (class fish%
    (public greeting)

    (init-field first-name)
    (init-field last-name)

    (define (greeting)
      (format "~a ~a" last-name first-name))

    (super-instantiate (12))))

;; Unused by-pos:
(err/rt-test (instantiate allow-rest/size-already-fish% ("Gil" "Finn" 18))
	     exn:object?)


;; Subclass where superclass has rest arg, check by-pos:

(define with-rest%
  (class object%
    (init-rest args)
    (field [a args])

    (public get-args)
    (define (get-args) a)
    (super-instantiate ())))

(define to-rest%
  (class with-rest%
    (super-instantiate ())))

(test '("hi" "there") 'to-rest (send (instantiate to-rest% ("hi" "there")) get-args))
(err/rt-test (instantiate to-rest% () (by-name "hi"))
	     exn:object?)

;; Check by-pos with super-instantiate:

(define to-rest2%
  (class with-rest%
    (super-instantiate ("hey,"))))

(test '("hey," "hi" "there") 'to-rest (send (instantiate to-rest2% ("hi" "there")) get-args))
(err/rt-test (instantiate to-rest2% () (by-name "hi"))
	     exn:object?)

;; Even more nested:

(define to-rest3%
  (class to-rest2%
    (super-instantiate ("um..."))))

(test '("hey," "um..." "hi" "there") 'to-rest (send (instantiate to-rest3% ("hi" "there")) get-args))

;; ------------------------------------------------------------
;; Test send/apply dotted send and method-call forms:

(define dotted% (class object%
		  (public f g)
		  (define (f x y z)
		    (list z y x))
		  (define (g x)
		    (let ([w (list x (add1 x) (+ x 2))])
		      (f . w)))
		  (super-make-object)))
(define dotted (make-object dotted%))
(test '(3 2 1) 'dotted (send dotted f 1 2 3))
(test '(9 8 7) 'dotted (send dotted g 7))
(let ([l (list 3 5 6)])
  (test '(6 5 3) 'dotted (send dotted f . l))
  (test '(6 5 3) 'dotted (send/apply dotted f l))
  (test '(9 8 7) 'dotted (send/apply dotted f '(7 8 9))))
(let ([l (list 6 8)])
  (test '(8 6 2) 'dotted (send dotted f 2 . l))
  (test '(8 6 2) 'dotted (send/apply dotted f 2 l))
  (test '(9 7 3) 'dotted (send/apply dotted f 3 '(7 9))))

(syntax-test #'(send/apply dotted f 2 . l))

;; ------------------------------------------------------------
;; Test init & feld external names

(define many-fields% (class object%
		       (init i1
			     [(i2* i2)])
		       (init-field i3
				   [(i4* i4)])
		       (init [i5 5]
			     [(i6* i6) 6])
		       (init-field (i7 7)
				   [(i8* i8) 8])
		       (field [a 10]
			      [(b* b) 12])
		       (define inits+fields (list i1 i2* i3 i4* i5 i6* i7 i8* a b*))
		       (define/public (get-fields)
			 (list i3 i4* i7 i8* a b*))
		       (define/public (get-inits+fields)
			 inits+fields)
		       (super-instantiate ())))

(let ([om1 (make-object many-fields% 10 20 30 40)]
      [oi1 (instantiate many-fields% () [i1 11] [i2 21] [i3 31] [i4 41])]
      [om2 (make-object many-fields% 12 22 32 42 52 62 72 82)]
      [oi2 (instantiate many-fields% () [i1 13] [i2 23] [i3 33] [i4 43] [i5 53] [i6 63] [i7 73] [i8 83])])
  (test '(10 20 30 40 5 6 7 8 10 12) 'om1-if (send om1 get-inits+fields))
  (test '(11 21 31 41 5 6 7 8 10 12) 'oi1-if (send oi1 get-inits+fields))
  (test '(30 40 7 8 10 12) 'om1-f (send om1 get-fields))
  (test '(31 41 7 8 10 12) 'oi1-f (send oi1 get-fields))
  (test '(12 22 32 42 52 62 72 82 10 12) 'om2-if (send om2 get-inits+fields))
  (test '(13 23 33 43 53 63 73 83 10 12) 'oi2-if (send oi2 get-inits+fields))
  (test '(32 42 72 82 10 12) 'om2-f (send om2 get-fields))
  (test '(33 43 73 83 10 12) 'oi2-f (send oi2 get-fields))
  (test 10 (class-field-accessor many-fields% a) om1)
  (test 12 (class-field-accessor many-fields% b) om1))


;; ------------------------------------------------------------
;; Test public*, define-public, etc.

(syntax-test #'(class object% public*))
(syntax-test #'(class object% (public* . x)))
(syntax-test #'(class object% (public* x)))
(syntax-test #'(class object% (public* [x])))
(syntax-test #'(class object% (public* [x . y])))
(syntax-test #'(class object% (public* [x 7 8])))
(syntax-test #'(class object% (public* [7 8])))

(syntax-test #'(class object% override*))
(syntax-test #'(class object% (override* . x)))
(syntax-test #'(class object% (override* x)))
(syntax-test #'(class object% (override* [x])))
(syntax-test #'(class object% (override* [x . y])))
(syntax-test #'(class object% (override* [x 7 8])))
(syntax-test #'(class object% (override* [7 8])))

(syntax-test #'(class object% private*))
(syntax-test #'(class object% (private* . x)))
(syntax-test #'(class object% (private* x)))
(syntax-test #'(class object% (private* [x])))
(syntax-test #'(class object% (private* [x . y])))
(syntax-test #'(class object% (private* [x 7 8])))
(syntax-test #'(class object% (private* [7 8])))

(syntax-test #'(class object% define/public))
(syntax-test #'(class object% (define/public)))
(syntax-test #'(class object% (define/public x)))
(syntax-test #'(class object% (define/public x 1 2)))
(syntax-test #'(class object% (define/public 1 2)))
(syntax-test #'(class object% (define/public (x 1) 2)))
(syntax-test #'(class object% (define/public (1 x) 2)))
(syntax-test #'(class object% (define/public (x . 1) 2)))
(syntax-test #'(class object% (define/public ((x 1) . a) 2)))
(syntax-test #'(class object% (define/public ((x b b) a) 2)))

(syntax-test #'(class object% define/override))
(syntax-test #'(class object% (define/override)))
(syntax-test #'(class object% (define/override x)))
(syntax-test #'(class object% (define/override x 1 2)))
(syntax-test #'(class object% (define/override 1 2)))
(syntax-test #'(class object% (define/override (x 1) 2)))
(syntax-test #'(class object% (define/override (1 x) 2)))
(syntax-test #'(class object% (define/override (x . 1) 2)))

(syntax-test #'(class object% define/private))
(syntax-test #'(class object% (define/private)))
(syntax-test #'(class object% (define/private x)))
(syntax-test #'(class object% (define/private x 1 2)))
(syntax-test #'(class object% (define/private 1 2)))
(syntax-test #'(class object% (define/private (x 1) 2)))
(syntax-test #'(class object% (define/private (1 x) 2)))
(syntax-test #'(class object% (define/private (x . 1) 2)))

(define c*1% (class object%
	       (define/public (x) (f))
	       (define/public ((higher-order a) b) (+ a b))
	       (public*
		[y (lambda () 2)]
		[z (lambda () 3)])
	       (private*
		[f (lambda () 1)])
	       (super-make-object)))

(define c*2% (class c*1%
	       (override*
		[y (lambda () 20)])
	       (define/override z (lambda () (g)))
	       (define/private (g) 30)
	       (super-make-object)))

(define o*1 (make-object c*1%))
(define o*2 (make-object c*2%))

(test 1 'o1 (send o*1 x))
(test 2 'o1 (send o*1 y))
(test 3 'o1 (send o*1 z))
(test 1 'o2 (send o*2 x))
(test 20 'o2 (send o*2 y))
(test 30 'o2 (send o*2 z))
(test 7 'o2 ((send o*2 higher-order 1) 6))

;; ----------------------------------------
;; Macro definitions in classes

(define cm1%
  (class object%
    (public meth)
    
    (define-syntax (macro stx)
      (syntax 10))
    
    (field [x (macro)])
    (init-field [y (macro)])
    (init  [-z (macro)])
    (field [z -z])

    (define w (macro))

    (define meth (lambda () (macro)))
    (define meth2 (lambda () (macro)))

    (define/public get-z (lambda () z))
    (define/public get-w (lambda () w))
    
    (super-instantiate ())))

(test 10 'cm1-meth (send (make-object cm1%) meth))
(test 10 'cm1-x ((class-field-accessor cm1% x) (make-object cm1%)))
(test 10 'cm1-y ((class-field-accessor cm1% y) (make-object cm1%)))
(test 10 'cm1-z (send (make-object cm1%) get-z))
(test 10 'cm1-w (send (make-object cm1%) get-w))

;; Make sure that local and syntax names do not shadow enclosing syntax for
;; definition RHSs
(test #t class? (let-syntax ([see-outer (lambda (x) (syntax (lambda () 10)))])
		  (class object%
		    (define see-outer 10)
		    (public meth)
		    (define meth (see-outer)))))
(test #t class? (let-syntax ([see-outer (lambda (x) (syntax (lambda () 10)))])
		  (class object%
		    (define-macro see-outer 10)
		    (public meth)
		    (define meth (see-outer)))))

;; Make sure that declared method names, field names, etc.
;; *do* shadow for definition RHSs
(let ([mk-syntax-test
       (lambda (mk)
	 (syntax-test (datum->syntax-object
		       (quote-syntax here)
		       `(let-syntax ([dont-see-outer (lambda (x) (syntax (lambda () 10)))])
			  (class object% 
			    ,@(mk 'dont-see-outer)
			    (public meth)
			    (define meth (dont-see-outer)))))))])
  (mk-syntax-test (lambda (id) `((init ,id))))
  (mk-syntax-test (lambda (id) `((init-rest ,id))))
  (mk-syntax-test (lambda (id) `((field [,id 10]))))
  (mk-syntax-test (lambda (id) `((inherit-field ,id))))
  (mk-syntax-test (lambda (id) `((inherit ,id))))
  (mk-syntax-test (lambda (id) `((rename [,id old-id]))))
  (mk-syntax-test (lambda (id) `((public ,id) (define (id) 10))))
  (mk-syntax-test (lambda (id) `((private ,id) (define (id) 10))))
  (mk-syntax-test (lambda (id) `((override ,id) (define (id) 10)))))


(syntax-test #'(class-field-accessor))
(syntax-test #'(class-field-accessor ok))
(syntax-test #'(class-field-accessor ok 7))
(syntax-test #'(class-field-accessor ok% ok ok))
(syntax-test #'(class-field-accessor ok% . ok))
(syntax-test #'(class-field-mutator))
(syntax-test #'(class-field-mutator ok))
(syntax-test #'(class-field-mutator ok 7))
(syntax-test #'(class-field-mutator ok% ok ok))
(syntax-test #'(class-field-mutator ok% . ok))

(syntax-test #'(define-local-member-name . a))
(syntax-test #'(define-local-member-name 7))
(syntax-test #'(define-local-member-name a 7))
(syntax-test #'(define-local-member-name a a))

;; ------------------------------------------------------
;; Private names

(let ([o (let ()
	   (define-local-member-name priv)
	   (let ([o (make-object
		     (class object%
		       (define/public (priv) (let ([priv 73]) priv))
		       (super-make-object)))])
	     (test 73 'priv (send o priv))
	     o))])
  (err/rt-test (send o priv) exn:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let ([c% (class object%
			(init-field priv)
			(super-make-object))])
	      (test 100 'priv ((class-field-accessor c% priv) (make-object c% 100)))
	      (test 100 'priv ((class-field-accessor c% priv) (instantiate c% () [priv 100])))
	      c%))])
  (err/rt-test (class-field-accessor c% priv) exn:object?)
  (test #t object? (make-object c% 10))
  (err/rt-test (instantiate c% () [priv 10]) exn:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let ([c% (class object%
			(init priv)
			(define xpriv priv)
			(define/public (m) xpriv)
			(super-make-object))])
	      (test 100 'priv (send (make-object c% 100) m))
	      (test 100 'priv (send (instantiate c% () [priv 100]) m))
	      c%))])
  (test 101 'priv (send (make-object c% 101) m))
  (err/rt-test (instantiate c% () [priv 101]) exn:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let ([c% (class object%
			(init xpriv)
			(field [priv xpriv])
			(define/public (m) priv)
			(super-make-object))])
	      (test 100 'priv ((class-field-accessor c% priv) (make-object c% 100)))
	      (test 101 'priv (send (make-object c% 101) m))
	      (test 100 'priv (send (instantiate c% () [xpriv 100]) m))
	      (test 100 'priv ((class-field-accessor c% priv) (instantiate c% () [xpriv 100])))
	      c%))])
  (err/rt-test (class-field-accessor c% priv) exn:object?)
  (test 101 'priv (send (make-object c% 101) m))
  (test 101 'priv (send (instantiate c% () [xpriv 101]) m))
  (err/rt-test (instantiate c% () [priv 10]) exn:object?))

(let ([c% (let ()
	    (define-local-member-name priv)
	    (let ([c% (class object%
			(init-field val)
			(define/public (priv) val)
			(super-make-object))])
	      (test 100 'priv (send (make-object c% 100) priv))
	      (test 100 'priv (send* (make-object c% 100) (priv)))
	      (test 100 'priv (with-method ([p ((make-object c% 100) priv)]) (p)))
	      (test 100 'gen-priv (send-generic (make-object c% 100) (generic c% priv)))
	      (err/rt-test (make-generic c% 'priv) exn:object?)
	      c%))])
  (test #t object? (make-object c% 10))
  (err/rt-test (send (make-object c% 10) priv) exn:object?)
  (err/rt-test (send* (make-object c% 10) (priv)) exn:object?)
  (err/rt-test (with-method ([p ((make-object c% 100) priv)]) (p)) exn:object?)
  (err/rt-test (generic c% priv) exn:object?)
  (err/rt-test (make-generic c% 'priv) exn:object?))

;; Make sure local name works with `send' in an area where the
;;  name is also directly bound:
(let ([c% (let ()
	    (define-local-member-name priv)
	    (class object%
	      (define/public (priv x) (+ x 10))
	      (define/public (pub y) (send this priv (* 2 y)))
	      (super-new)))])
  (test 16 'send-using-local (send (new c%) pub 3)))

;; ------------------------------------------------------------
;; class*/names rebinds names

(test #t class? (class*/names (thiss super-initt super-moo) object% ()
                  (super-initt ())))

(test #t object? (make-object (class*/names (thiss super-initt super-moo) object% ()
                                (super-initt ()))))

(test #t object? (make-object (class*/names (thiss super-initt super-moo) object% ()
                                (apply super-moo '()))))

(test #t object? (make-object (class*/names (thiss super-initt super-moo) object% ()
                                thiss
                                (super-initt ()))))
(test #t object? (make-object (class*/names (thiss super-initt super-moo super-novel) object% ()
                                thiss
                                (super-novel))))

(test #t boolean? 
      (send (make-object (class*/names (thiss super-initt super-moo) object% ()
                           (define/public (m x) x)
                           (define/public (n x) (send thiss m x))
                           (super-initt ())))
            n #t))


;; ------------------------------------------------------------
;; `new' tests

(syntax-test #'(new))
(syntax-test #'(new x x))
(syntax-test #'(new x ()))
(syntax-test #'(new x (x)))
(syntax-test #'(new x ("a" x)))

(test #t object? (new object%))
(test #t object? (new (class object% () (init-field x) (super-instantiate ())) (x 1)))

;; ------------------------------------------------------------
;; `field' tests


(syntax-test #'(get-field))
(syntax-test #'(get-field a))
(syntax-test #'(get-field 1 b))
(syntax-test #'(get-field a b c))

(error-test #'(get-field x 1) exn:application:mismatch?)
(error-test #'(get-field x (new object%)) exn:application:mismatch?)
(error-test #'(get-field x (new (class object% (define x 1) (super-new))))
            exn:application:mismatch?)
(error-test #'(let ([o (let ()
                         (define-local-member-name f)
                         (new (class object%
                                (field [f 0])
                                (super-new))))])
                (get-field f o))
            exn:application:mismatch?)
(test 0 'get-field1 (get-field x (new (class object% (field [x 0]) (super-new)))))
(test 0 'get-field2 (let ()
                      (define-local-member-name f)
                      (get-field f (new (class object% (field [f 0]) (super-new))))))

(syntax-test #'(field-bound?))
(syntax-test #'(field-bound? a))
(syntax-test #'(field-bound? 1 b))
(syntax-test #'(field-bound? a b c))

(error-test #'(field-bound? x 1) exn:application:mismatch?)
(test #t 'field-bound?1 (field-bound? x (new (class object% (field [x 0]) (super-new)))))
(test #f 'field-bound?2 (field-bound? y (new (class object% (field [x 0]) (super-new)))))
(test #f 'field-bound?3 (field-bound? y (new object%)))

(test #f
      'field-bound?/local-name1
      (let ([o (let ()
                 (define-local-member-name f)
                 (new (class object% (field [f 10]) (super-new))))])
        (field-bound? f o)))

(test #t
      'field-bound?/local-name2
      (let ()
        (define-local-member-name f)
        (field-bound? f (new (class object% (field [f 10]) (super-new))))))

(test '(f) field-names (new (class object% (field [f 1]) (super-new))))
(test '(g)
      field-names 
      (let ()
        (define-local-member-name f)
        (new (class object% (field [f 1] [g 1]) (super-new)))))

(report-errs)
