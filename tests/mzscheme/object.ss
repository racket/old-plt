
; Test MzScheme's new object system

(load-relative "loadtest.ss")

(require (lib "class.ss"))

(SECTION 'OBJECT)

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

(define fish-size (make-class-field-accessor fish% 'size))
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

    (super-instantiate ())))

(define blue-fish (instantiate color-fish% () (color 'blue) (size 10)))
(define red-fish (instantiate color-fish% () (size 1)))

(define color-fish-color (make-class-field-accessor color-fish% 'color))

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

    (super-instantiate () (size 12))))

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
(test 10 'cm1-x ((make-class-field-accessor cm1% 'x) (make-object cm1%)))
(test 10 'cm1-y ((make-class-field-accessor cm1% 'y) (make-object cm1%)))
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

(report-errs)
