#|

Rough BNF

(class/d
 super-expresion
 init-args
 ((public var ...)
  (override var ...)
  (inherit var ...)
  (rename (var var) ...))
 
  definitions-and-expressions ...)

;; only thing wrong with above bnf is that the public, etc. clauses
;; can appear multiple times in that same section. 

|#

(require-library "refer.ss")
(begin-elaboration-time
 (define-values/invoke-unit
  (class/d class/d* class/d*/names)
   (require-library "classdr.ss")))

(define-macro class/d class/d)
(define-macro class/d* class/d*)
(define-macro class/d*/names class/d*/names)

#|

;; Tests

(define (synerr x)
  (with-handlers ([exn:syntax? (lambda (x) (void))])
    (eval x)
    (error 'synerr "not a syntax error: ~s" x)))

(define (test tag expected t)
  (let ([got 
	 (with-handlers ([(lambda (x) #t)
			  (lambda (x) (exn-message x))])
	   (eval t))])
    (unless (equal? expected got)
      (error 'test "~a: expected ~s got ~s" tag expected got))))

(synerr '(class/d object% ((public x)) (define (x) 1)))
(synerr '(class/d object% () ((public x))))

(test
 1
 1
 '(send (make-object (class/d object% () ((public y)) (define (y) 1) (super-init))) y))

(test
 2
 1
 '(send (make-object (class/d object% () ((public y)) (define (y) 1) (define (z) 1) (super-init))) y))

(test
 3
 3
 '(let ([x 1])
   (make-object 
       (class/d object% () ()
                (set! x 2)
                (set! x 3)
                (super-init)))
   x))

(test
 4
 2
 '(send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((override x))
                             (super-init)
                             (define (x) 2)))
       x))


(test
 5
 2
 '(send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((inherit x)
                              (public y))
                             (super-init)
                             (define (y) (+ (x) (x)))))
       y))

(test
 6
 2
 '(send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((rename [super-x x])
                              (public y))
                             (super-init)
                             (define (y) (+ (super-x) (super-x)))))
       y))

(test
 7
 2
 '(send (make-object (class/d (class object% () (public [x (lambda () 1)]) (sequence (super-init)))
                             ()
                             ((rename [super-x x])
                              (override x))
                             (super-init)
                             (define (x) (+ (super-x) (super-x)))))
       x))

(test
 8
 2
 '(send (make-object (class/d object% (xxx)
		      ((public x))
		      (define (x) xxx)
		      (super-init))
	 2)
       x))

(test
 9
 1
 '(send (make-object (class/d*/names (local-this local-super-init)
				    object%
				    ((interface ()))
				    ()
				    ((public x))
				    (define (x) 1)
				    (local-super-init)))
       x))

(test
 10
 1
 '(send (make-object (class/d* object%
			      ((interface ()))
			      ()
			      ((public x))
			      (define (x) 1)
			      (super-init)))
       x))

|#
